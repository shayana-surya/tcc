#include <rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix createEuclideanDistanceRcpp(NumericMatrix CemigData,int n) {
  NumericMatrix matrizDistancia(n,n);
  double diffx;
  double diffy;
  double dist;
  for (int i=0; i < n; ++i) {
    for (int j=0; j < n; ++j) {
      
      diffx = CemigData(i,1) - CemigData(j,1);
      diffy = CemigData(i,2) - CemigData(j,2);
      dist = pow(diffx, 2) + pow(diffy, 2);
      
      matrizDistancia(i,j) = sqrt(dist);
    }
  }
  return (matrizDistancia);
}
    
    
    //NumericMatrix createIndexMatrixRcpp(NumericMatrix CemigData,int n) {
    //  NumericMatrix matIDX(n,n);
    //  NumericVector aux (n);
    //  for (int i= 0; i < n; ++i) {
    //    aux = CemigData(i,0);
    //    matIDX(i,0) = sort(aux.begin(), aux.end());
    //  }
    //  return (matIDX);
    //}