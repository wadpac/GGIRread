#include <Rcpp.h>
#include <fstream>

// [[Rcpp::export]]
Rcpp::IntegerVector find_matrix_packet_start(std::string file_path, Rcpp::RawVector pattern, int chunk_size = 1048576) {
  std::ifstream file(file_path, std::ios::binary);
  if (!file) {
    Rcpp::stop("Failed to open file: '%s'", file_path);
  }
  
  int pattern_len = pattern.size();
  std::vector<int> indices;
  std::vector<unsigned char> buffer(chunk_size + pattern_len - 1);  // Extra space for overlap
  
  std::streampos position = 0;  // Keeps track of the file position
  while (file) {
    file.read(reinterpret_cast<char*>(buffer.data()), chunk_size);
    std::streamsize bytesRead = file.gcount();
    if (bytesRead == 0) break;  // Stop if no more bytes read
    
    // Ensure we don't compare signed and unsigned types
    size_t search_limit = static_cast<size_t>(std::max(static_cast<std::streamoff>(bytesRead) - pattern_len, static_cast<std::streamoff>(0)));
    
    // Search for pattern in the current buffer
    for (size_t i = 0; i <= search_limit; ++i) {
      bool match = true;
      for (size_t j = 0; j < static_cast<size_t>(pattern_len); ++j) {
        if (buffer[i + j] != pattern[j]) {
          match = false;
          break;
        }
      }
      if (match) {
        indices.push_back(static_cast<int>(static_cast<std::streamoff>(position) + i + 1)); // Convert to 1-based index
      }
    }
    
    // Move back the last `pattern_len - 1` bytes for next chunk to prevent missed matches
    if (bytesRead >= pattern_len - 1) {
      file.seekg(position + static_cast<std::streamoff>(bytesRead) - static_cast<std::streamoff>(pattern_len - 1));
      position = file.tellg();
    } else {
      break; // If not enough bytes left, end
    }
  }
  
  return Rcpp::wrap(indices);
}
