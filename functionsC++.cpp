#include <algorithm>
#include <vector>
#include <random>       // std::default_random_engine
#include <chrono>       // std::chrono::system_clock
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
NumericMatrix geradorClustersRcpp(NumericMatrix matrizDistancia, NumericVector consumo,int N, int raio){
  
  //int col = type + 2; // indica a coluna em diffCemigData que contem o consumo a ser considerado na analise
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
        consumoIn = consumoIn + consumo(j);
        numIn = numIn + 1;
      }
      else{
        consumoOut = consumoOut + consumo(j);
        numOut = numOut + 1;
      }
    }
    
    miz = consumoIn/numIn;
    lambdaz = consumoOut/numOut;
    
    for(int j = 0; j < N; j++){
      if(matrizDistancia(i,j) <= raio){
        parteIn = parteIn + pow((consumo(j) - miz),2);
      }
      else{
        parteOut = parteOut + pow((consumo(j) - lambdaz),2);
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
double SimulClustersRcpp(NumericMatrix matrizDistancia,NumericVector consumo,int N, int raio){
  
  //int col = type + 2; // indica a coluna em diffCemigData que contem o consumo a ser considerado na analise
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
        consumoIn = consumoIn + consumo(j);
        numIn = numIn + 1;
      }
      else{
        consumoOut = consumoOut + consumo(j);
        numOut = numOut + 1;
      }
    }
    
    miz = consumoIn/numIn;
    lambdaz = consumoOut/numOut;
    
    for(int j = 0; j < N; j++){
      if(matrizDistancia(i,j) <= raio){
        parteIn = parteIn + pow((consumo(j) - miz),2);
      }
      else{
        parteOut = parteOut + pow((consumo(j) - lambdaz),2);
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

// [[Rcpp::export]]
NumericVector MonteCarloRcpp(NumericMatrix matrizDistancia,NumericVector consumo,int N, int raio, int bound){
  
  NumericVector resultSimul(bound);
  
  for(int i = 0; i < bound; i++){
    
    NumericVector dados = clone(consumo);
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::shuffle(dados.begin(),dados.end(),std::default_random_engine(seed));
    
    resultSimul(i) = SimulClustersRcpp(matrizDistancia,dados,N,raio);
  }
  
  return(resultSimul);
}