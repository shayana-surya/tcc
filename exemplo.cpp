#include <rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. 
// [[Rcpp::export]]
NumericMatrix createEuclideanDistanceRcpp(NumericMatrix matrizDistancia,NumericMatrix CemigData,int n) {
  
  int x = 0;
  double diffx;
  double diffy;
  double dist;
  for (int i=0; i < n; ++i) {
    for (int j=0; j < n; ++j) {
      
      diffx = CemigData(i,0) - CemigData(j,0);
      diffy = CemigData(i,1) - CemigData(j,1);
      dist = pow(diffx, 2) + pow(diffy, 2);
      
      matrizDistancia(x,0) = CemigData(i,3);
      matrizDistancia(x,1) = CemigData(j,3);
      matrizDistancia(x,2) = 0;
      matrizDistancia(x,3) = sqrt(dist);
      ++x;
    }
  }
  return (matrizDistancia);
}