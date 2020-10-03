#include <algorithm>
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

// [[Rcpp::export]]
NumericMatrix geradorClustersRcpp(NumericMatrix matrizDistancia, NumericMatrix diffCemigData,int N, int raio){
  
  NumericMatrix clusters(N,3); // matriz para guardar os clusters gerados: Centroide; Qtde de alimentadores; Estatística

  for(int i = 0; i < N; i++){ // Objetivo: gerar um cluster para cada alimentador na função de centroide
    
    double consumoIn = 0;
    double consumoOut = 0;
    double miz = 0;
    double lambdaz = 0;
    double sigma2z = 0;
    double parteIn = 0;
    double parteOut = 0;
    double estatistica = 0;
    int numIn = 0;
    int numOut = 0;
    
    // acessar primeria linha da matriz de distancia
    // testo se a distancia na coluna y é menor ou igual ao raio
    // se for eu salvo o y no vetor x.in, c.c. salvo no vetor x.out
    for(int j = 0; j < N; j++){
      if(matrizDistancia(i,j) <= raio){
        consumoIn = consumoIn + diffCemigData(j,3);
        numIn = numIn + 1;
      }
      else{
        consumoOut = consumoOut + diffCemigData(j,3);
        numOut = numOut + 1;
      }
    }
    
    miz = consumoIn/numIn;
    lambdaz = consumoOut/numOut;
    
    for(int j = 0; j < N; j++){
      if(matrizDistancia(i,j) <= raio){
        parteIn = parteIn + pow((diffCemigData(j,3) - miz),2);
      }
      else{
        parteOut = parteOut + pow((diffCemigData(j,3) - lambdaz),2);
      }
    }
    
    sigma2z = (parteIn + parteOut)/N;
    estatistica = -N*log(sigma2z)/2;
    
    clusters(i,0) = i+1;
    clusters(i,1) = numIn;
    clusters(i,2) = estatistica;
    
  }
  
  return(clusters);
}

// [[Rcpp::export]]
double SimulClustersRcpp(NumericMatrix matrizDistancia, NumericMatrix diffCemigData,int N, int raio){
  
  double bestResult = 0;
  
  for(int i = 0; i < N; i++){ // Objetivo: gerar um cluster para cada alimentador na função de centroide
    
    double consumoIn = 0;
    double consumoOut = 0;
    double miz = 0;
    double lambdaz = 0;
    double sigma2z = 0;
    double parteIn = 0;
    double parteOut = 0;
    double estatistica = 0;
    int numIn = 0;
    int numOut = 0;
    
    // acessar primeria linha da matriz de distancia
    // testo se a distancia na coluna y é menor ou igual ao raio
    // se for eu salvo o y no vetor x.in, c.c. salvo no vetor x.out
    for(int j = 0; j < N; j++){
      if(matrizDistancia(i,j) <= raio){
        consumoIn = consumoIn + diffCemigData(j,3);
        numIn = numIn + 1;
      }
      else{
        consumoOut = consumoOut + diffCemigData(j,3);
        numOut = numOut + 1;
      }
    }
    
    miz = consumoIn/numIn;
    lambdaz = consumoOut/numOut;
    
    for(int j = 0; j < N; j++){
      if(matrizDistancia(i,j) <= raio){
        parteIn = parteIn + pow((diffCemigData(j,3) - miz),2);
      }
      else{
        parteOut = parteOut + pow((diffCemigData(j,3) - lambdaz),2);
      }
    }
    
    sigma2z = (parteIn + parteOut)/N;
    estatistica = -N*log(sigma2z)/2;
    
    if(bestResult == 0 || estatistica > bestResult){
      bestResult = estatistica;
    }
    
  }
 
  return(bestResult);
}

/*// [[Rcpp::export]]
double* MonteCarloRcpp(NumericMatrix matrizDistancia, NumericMatrix diffCemigData,int N, int raio, int bound){
  
  double resultSimul[bound];
  
  for(int i = 0; i < bound; i++){
    NumericMatrix dados = diffCemigData;
    std::shuffle(std::begin(dados(0,3)),std::end(dados(N,3));
    resultSimul[i] <- SimulClustersRcpp(matrizDistancia,dados,N,raio);
  }
  
  return(resultSimul);
}*/