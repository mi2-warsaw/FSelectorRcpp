#ifndef FSELECTOR_CONFIG_INCLUDES
#define FSELECTOR_CONFIG_INCLUDES

// #ifdef FS_OUTPUT
// #include <iostream>
// #define FS_OUTPUT std::cout
#ifndef FS_OUTPUT
  #define FS_OUTPUT Rcpp::Rcout
#endif

#endif
