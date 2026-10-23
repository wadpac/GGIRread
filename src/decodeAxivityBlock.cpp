#include <Rcpp.h>
#include <cstdint>
#include <cmath>

namespace {
// Decode explicitly: CWA packets are little endian and need not be aligned.
unsigned int u16(const Rcpp::RawVector& b, int offset) {
  return static_cast<unsigned int>(b[offset]) |
    (static_cast<unsigned int>(b[offset + 1]) << 8);
}

int s16(const Rcpp::RawVector& b, int offset) {
  const unsigned int value = u16(b, offset);
  return value >= 0x8000U ? static_cast<int>(value) - 0x10000 : value;
}

std::uint32_t u32(const Rcpp::RawVector& b, int offset) {
  return static_cast<std::uint32_t>(u16(b, offset)) |
    (static_cast<std::uint32_t>(u16(b, offset + 2)) << 16);
}

int signed10(std::uint32_t value) {
  const int sample = static_cast<int>(value & 0x3ffU);
  return sample >= 0x200 ? sample - 0x400 : sample;
}
}

// Packet decoding only. Calendar/timezone conversion and the inter-packet
// integrity, imputation and interpolation rules remain in readAxivity().
// CWA layout: https://github.com/digitalinteraction/openmovement/blob/master/Docs/ax3/cwa.h
// [[Rcpp::export]]
Rcpp::List decodeAxivityBlock(Rcpp::RawVector block, bool complete,
                             Rcpp::Nullable<Rcpp::List> parameters,
                             bool loadbattery) {
  using namespace Rcpp;
  if (block.size() != 512) stop("Axivity data blocks must contain 512 bytes.");

  const int samplerate = block[24];
  if (samplerate != 0) {
    unsigned int checksum = 0;
    for (int offset = 0; offset < 512; offset += 2) checksum += u16(block, offset);
    if ((checksum & 0xffffU) != 0) return List::create(_["checksum_pass"] = false);
  }
  if (block[0] != 'A' || block[1] != 'X') {
    stop("Packet header is incorrect. First two characters must be AX.");
  }
  if (u16(block, 2) != 508) {
    stop("Packet length is incorrect, should always be 508.");
  }

  const unsigned int tsOffset = u16(block, 4);
  const unsigned int lightScale = u16(block, 18);
  const int axesPacking = block[25];
  const int packing = axesPacking & 15;
  if (packing != 0 && packing != 2) stop("Unsupported Axivity sample packing format.");
  const bool packed = packing == 0;
  const int offset26 = s16(block, 26);
  const int count = u16(block, 28);
  const bool reuse = parameters.isNotNull();
  List params;
  int axes, scaleCode, format;
  double accelScale, frequency;
  if (reuse) {
    params = List(parameters.get());
    axes = as<int>(params["Naxes"]);
    scaleCode = as<int>(params["accelScaleCode"]);
    accelScale = as<double>(params["accelScale"]);
    frequency = as<double>(params["frequency_data"]);
    format = as<int>(params["format"]);
  } else {
    axes = axesPacking >> 4;
    scaleCode = lightScale >> 13;
    accelScale = 1.0 / std::pow(2.0, 8 + scaleCode);
    frequency = samplerate == 0 ? offset26 :
      3200.0 / (1U << (15 - (samplerate & 15)));
    format = samplerate == 0 ? 0 : ((tsOffset & 0x8000U) != 0 ? 1 : 2);
    params = List::create(_["accelScaleCode"] = scaleCode,
                          _["accelScale"] = accelScale,
                          _["Naxes"] = axes,
                          _["frequency_data"] = frequency,
                          _["format"] = static_cast<double>(format));
  }
  // Old-format packets carry their frequency in each packet, even when the
  // other decoding parameters are reused from the preceding packet.
  if (samplerate == 0) frequency = offset26;
  if (axes != 3 && axes != 6) stop("Unsupported number of Axivity axes.");
  if (packed && axes != 3) stop("Packed Axivity data must have three axes.");
  const int capacity = packed ? 120 : 480 / (2 * axes);
  if (count < 1 || count > capacity) stop("Invalid Axivity sample count for packet format.");
  if (!std::isfinite(frequency) || frequency <= 0) stop("Invalid Axivity sampling frequency.");

  int fractional = 0;
  double shift = 0;
  if (samplerate != 0) {
    shift = offset26;
    if (format == 1) {
      // Undo the firmware's backwards-compatible timestamp shim: the lower
      // 15 bits hold a fraction of a second, and offset26 includes its samples.
      // The firmware truncates the frequency to an integer here (e.g. 12 for
      // 12.5 Hz), so undo the shim with the same truncated value.
      fractional = (tsOffset & 0x7fffU) << 1;
      shift += std::floor(fractional * std::floor(frequency) / 65536.0);
    }
  }
  // Temperature uses the low ten ADC bits. Battery stores only the middle
  // eight ADC bits; restoring the implicit high bit gives 3 * (byte/256 + 1).
  List result = List::create(
    _["frequency"] = frequency,
    _["temperature"] = (u16(block, 20) & 0x3ffU) * 75.0 / 256.0 - 50.0,
    _["battery"] = loadbattery ? 3.0 * (block[23] / 256.0 + 1.0) : 0.0,
    _["light"] = static_cast<int>(lightScale & 0x3ffU),
    _["length"] = count,
    _["parameters"] = params,
    _["checksum_pass"] = true,
    _["blockID"] = static_cast<std::int32_t>(u32(block, 10)),
    _["timeStamp"] = static_cast<std::int32_t>(u32(block, 14)),
    _["fractional"] = fractional,
    _["shift"] = -shift / frequency);
  if (complete) {
    NumericMatrix data(count, axes);
    // Per Daniel Jackson's AX6 clarification, public firmware uses gyro codes
    // 2-6 (2000-125 dps); code 0 means disabled gyro in three-axis packets.
    // Keep the formula for unused codes as an extension of the range pattern.
    // omconvert's code-0 fallback to 2000 dps concerns pre-release prototypes.
    const double gyroRange = 8000.0 / (1U << ((lightScale >> 10) & 7U));
    for (int row = 0; row < count; ++row) {
      if (packed) {
        const std::uint32_t value = u32(block, 30 + 4 * row);
        const int multiplier = 1 << (value >> 30);
        for (int axis = 0; axis < 3; ++axis) {
          // Multiplication avoids left-shifting a negative signed integer.
          data(row, axis) = signed10(value >> (10 * axis)) * multiplier * accelScale;
        }
      } else {
        for (int axis = 0; axis < axes; ++axis) {
          const int sample = s16(block, 30 + 2 * (row * axes + axis));
          data(row, axis) = axes == 6 && axis < 3 ?
            (sample / 32768.0) * gyroRange : sample * accelScale;
        }
      }
    }
    colnames(data) = axes == 3 ? CharacterVector::create("x", "y", "z") :
      CharacterVector::create("gx", "gy", "gz", "x", "y", "z");
    result["data"] = data;
  }
  return result;
}
