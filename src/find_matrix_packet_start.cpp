#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_matrix_packet_start(RawVector data, RawVector pattern) {
  int data_len = data.size();
  int pattern_len = pattern.size();
  IntegerVector indices;
  
  for (int i = 0; i <= data_len - pattern_len; i++) {
    bool match = true;
    for (int j = 0; j < pattern_len; j++) {
      if (data[i + j] != pattern[j]) {
        match = false;
        break;
      }
    }
    if (match) {
      indices.push_back(i + 1); // Use 1-based index for R
    }
  }
  return indices;
}
