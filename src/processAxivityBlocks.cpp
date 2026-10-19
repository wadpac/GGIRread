#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

Rcpp::List decodeAxivityBlock(Rcpp::RawVector, bool,
                             Rcpp::Nullable<Rcpp::List>, bool);

namespace {
struct Block {
  double start, frequency, temperature, battery, light;
  int length, id;
  Rcpp::NumericMatrix data;
  Block(const Rcpp::List& x, double timestamp) :
    start(timestamp), frequency(Rcpp::as<double>(x["frequency"])),
    temperature(Rcpp::as<double>(x["temperature"])),
    battery(Rcpp::as<double>(x["battery"])), light(Rcpp::as<double>(x["light"])),
    length(Rcpp::as<int>(x["length"])), id(Rcpp::as<int>(x["blockID"])),
    data(Rcpp::as<Rcpp::NumericMatrix>(x["data"])) {}
};

struct QC {
  bool checksum, imputed;
  int current, next;
  double start, end, duration, expected, observed;
};

SEXP qualityLog(const std::vector<QC>& rows) {
  if (rows.empty()) return R_NilValue;
  const R_xlen_t n = rows.size();
  Rcpp::LogicalVector checksum(n), imputed(n);
  Rcpp::IntegerVector current(n), next(n);
  Rcpp::NumericVector start(n), end(n), duration(n), expected(n), observed(n);
  bool hasCorrupt = false;
  for (R_xlen_t i = 0; i < n; ++i) {
    const QC& row = rows[i];
    hasCorrupt = hasCorrupt || !row.checksum;
    checksum[i] = row.checksum; imputed[i] = row.imputed;
    current[i] = row.current; next[i] = row.next;
    start[i] = row.start; end[i] = row.end; duration[i] = row.duration;
    expected[i] = row.expected; observed[i] = row.observed;
  }
  // The R checksum-failure rows use ii + 1 (a double) for blockID_next;
  // otherwise both IDs come directly from integer packet headers.
  Rcpp::RObject nextIDs = hasCorrupt ?
    Rcpp::RObject(Rcpp::wrap(Rcpp::as<Rcpp::NumericVector>(next))) :
    Rcpp::RObject(Rcpp::wrap(next));
  return Rcpp::DataFrame::create(
    Rcpp::_["checksum_pass"] = checksum,
    Rcpp::_["blockID_current"] = current, Rcpp::_["blockID_next"] = nextIDs,
    Rcpp::_["start"] = start, Rcpp::_["end"] = end,
    Rcpp::_["blockLengthSeconds"] = duration,
    Rcpp::_["frequency_blockheader"] = expected,
    Rcpp::_["frequency_observed"] = observed, Rcpp::_["imputed"] = imputed);
}
}

// Process one requested chunk. R retains the file connection (including Unicode
// paths and connection cleanup), endpoint selection and calendar/timezone rules.
// readBlocks supplies a bounded byte buffer; calendar is called only when the
// cached whole-minute timestamp changes or the device sequence resets.
// [[Rcpp::export]]
Rcpp::List processAxivityBlocks(Rcpp::Function readBlocks, Rcpp::Function calendar,
                               Rcpp::Nullable<Rcpp::Function> progress,
                               Rcpp::List first, Rcpp::NumericVector time,
                               double start, double end, double numDBlocks,
                               double frequency_tol, int maxAllowedCorruptBlocks,
                               bool loadbattery, int interpolationType) {
  using namespace Rcpp;
  if (time.size() == 0 || time.size() > std::numeric_limits<int>::max()) {
    stop("Invalid number of samples in Axivity chunk.");
  }
  const int nr = time.size();
  Block previous(first, as<double>(first["start"]));
  List parameters = first["parameters"];
  const int axes = as<int>(parameters["Naxes"]);
  List state = first["struc"];
  double timestamp = as<double>(state[0]);
  int oldSeconds = as<int>(state[1]);
  unsigned int oldMinute = as<unsigned int>(state[2]);
  NumericMatrix accel(nr, axes);
  colnames(accel) = axes == 3 ? CharacterVector::create("x", "y", "z") :
    CharacterVector::create("gx", "gy", "gz", "x", "y", "z");
  NumericVector temperature(nr), battery(nr), light(nr);
  std::vector<QC> qc;
  Function sum = Environment::base_env()["sum"];
  Function power = Environment::base_env()["^"];
  Function squareRoot = Environment::base_env()["sqrt"];
  std::vector<double> anchor(axes, 0.0), rawTime;
  double anchorTime = 0, prevStart = 0;
  int prevLength = 0, position = 0, consecutiveCorrupt = 0;
  bool firstIteration = true;
  RawVector buffer(0), packet(512);
  R_xlen_t offset = 0;

  for (double ii = start + 1; ii <= end; ++ii) {
    if (static_cast<R_xlen_t>(ii - start) % 128 == 0) checkUserInterrupt();
    Block current = previous;
    double nextTime;
    if (ii == numDBlocks) {
      // Estimate the final packet's duration from the preceding packet, or
      // use its nominal frequency when it is the only requested packet.
      if (firstIteration) {
        prevStart = previous.start;
        nextTime = prevStart + previous.length / previous.frequency;
      } else {
        nextTime = (previous.start - prevStart) / prevLength * previous.length + previous.start;
      }
      prevStart = previous.start;
      prevLength = previous.length;
    } else {
      if (offset + 512 > buffer.size()) {
        buffer = as<RawVector>(readBlocks());
        offset = 0;
      }
      if (offset + 512 > buffer.size()) {
        stop("\nreadAxivity encountered unexpected empty block at #%.0f", ii);
      }
      std::copy(buffer.begin() + offset, buffer.begin() + offset + 512, packet.begin());
      offset += 512;
      List decoded = decodeAxivityBlock(packet, true, parameters, loadbattery);
      if (!as<bool>(decoded["checksum_pass"])) {
        qc.push_back(QC{false, false, static_cast<int>(ii), static_cast<int>(ii + 1),
                        0, 0, 0, 0, 0});
        warning("Skipping corrupt block #%.0f", ii);
        if (++consecutiveCorrupt > maxAllowedCorruptBlocks) {
          stop("Error reading file. %d consecutive blocks are corrupt.", maxAllowedCorruptBlocks + 1);
        }
        continue;
      }
      consecutiveCorrupt = 0;
      const int coded = as<int>(decoded["timeStamp"]);
      const unsigned int minute = static_cast<unsigned int>(coded) >> 6;
      const int seconds = static_cast<unsigned int>(coded) & 0x3fU;
      if (as<int>(decoded["blockID"]) == 0 || minute != oldMinute || timestamp == 0) {
        timestamp = as<double>(calendar(coded));
      } else {
        if (seconds < oldSeconds) oldSeconds -= 60;
        timestamp += seconds - oldSeconds;
      }
      oldMinute = minute;
      oldSeconds = seconds;
      // Match timestampDecoder's arithmetic order, including the fraction.
      const double blockStart = timestamp + as<double>(decoded["fractional"]) / 65536.0 +
        as<double>(decoded["shift"]);
      current = Block(decoded, blockStart);
      prevStart = previous.start;
      prevLength = previous.length;
      nextTime = current.start;
    }

    // Equivalent to seq(prevStart, nextTime, length.out = prevLength + 1).
    // The endpoint is a time anchor, not a sample supplied to interpolation.
    if (!std::isfinite(prevStart)) stop("'from' must be a finite number");
    if (!std::isfinite(nextTime)) stop("'to' must be a finite number");
    const int length = prevLength;
    rawTime.resize(length + 2);
    const double spacing = (nextTime - prevStart) / length;
    rawTime[1] = prevStart;
    for (int i = 1; i < length; ++i) rawTime[i + 1] = prevStart + i * spacing;
    rawTime[length + 1] = nextTime;
    if (firstIteration) {
      for (int axis = 0; axis < axes; ++axis) anchor[axis] = previous.data(0, axis);
      anchorTime = prevStart - 0.00001;
      firstIteration = false;
    }
    rawTime[0] = anchorTime;
    double observed = (length + 1.0) / (rawTime[length] - anchorTime);
    if (ii == numDBlocks && start == numDBlocks - 1) observed = previous.frequency;
    const double bias = std::abs(observed - previous.frequency) / previous.frequency;
    const bool impute = (ii < numDBlocks && current.id != 0 &&
                         static_cast<double>(current.id) - previous.id != 1) || bias > frequency_tol;
    if (impute || bias > 0.05) {
      qc.push_back(QC{true, impute, previous.id, current.id, previous.start, current.start,
                      current.start - previous.start, previous.frequency, observed});
    }
    int last = std::min<R_xlen_t>(static_cast<R_xlen_t>(position) + 200, nr - 1);
    if (rawTime[length] > time[last]) {
      const double add = std::floor((rawTime[length] - time[last]) * previous.frequency);
      last = static_cast<int>(std::min(static_cast<double>(last) + add, static_cast<double>(nr - 1)));
    }
    if (impute && last - position > previous.frequency * 259200) {
      Function round = Environment::base_env()["round"];
      Function paste0 = Environment::base_env()["paste0"];
      stop(as<std::string>(paste0("\nreadAxivity encountered a time gap in the file of ",
        round((last - position) / (3600.0 * 24) / previous.frequency, 2), " days")));
    }
    double imputed[3] = {0, 0, 1};
    if (impute) {
      // Retain R's platform-specific summation for the three-value norm. This
      // rare callback avoids changing repeated normalisation by one ULP.
      NumericVector orientation(3);
      for (int axis = 0; axis < 3; ++axis) {
        orientation[axis] = anchor[axis];
      }
      const double norm = as<double>(squareRoot(sum(power(orientation, 2.0))));
      if (norm > 0.8 && norm < 1.2) {
        for (int axis = 0; axis < 3; ++axis) imputed[axis] = anchor[axis] / norm;
      }
    }
    int source = 1;
    while (position <= last && (impute ? time[position] < rawTime[length] :
                               time[position] <= rawTime[length])) {
      if (impute) {
        for (int axis = 0; axis < axes; ++axis) accel(position, axis) = axis < 3 ? imputed[axis] : 0;
      } else {
        while (source < length && rawTime[source] < time[position]) ++source;
        const double weight = (time[position] - rawTime[source - 1]) /
          (rawTime[source] - rawTime[source - 1]);
        for (int axis = 0; axis < axes; ++axis) {
          const double left = source == 1 ? anchor[axis] : previous.data(source - 2, axis);
          const double right = previous.data(source - 1, axis);
          if (interpolationType == 1) accel(position, axis) = weight * (right - left) + left;
          else if (interpolationType == 2) accel(position, axis) = std::ceil(weight - 0.5) * (right - left) + left;
        }
      }
      temperature[position] = previous.temperature;
      battery[position] = previous.battery;
      light[position] = previous.light;
      ++position;
    }
    if (position == 0) stop("No samples could be resampled from the Axivity block.");
    anchorTime = time[position - 1];
    for (int axis = 0; axis < axes; ++axis) anchor[axis] = accel(position - 1, axis);
    previous = current;
    if (progress.isNotNull()) Function(progress.get())(position + 1);
  }
  if (position == 0) stop("No samples could be resampled from the Axivity chunk.");
  RObject log = qualityLog(qc);
  return List::create(_["accel"] = accel, _["temp"] = temperature,
                       _["battery"] = battery, _["light"] = light,
                       _["last"] = position, _["QClog"] = log);
}
