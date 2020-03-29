#include <rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. 
// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}