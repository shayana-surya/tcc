#include <algorithm>
#include <vector>
#include <random>
#include <chrono>
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
NumericMatrix clusterGeneratorRadiusRcpp(NumericMatrix matrizDistancia, NumericVector consumo,int N, int raio){
  
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
double clusterGeneratorRadiusSimulRcpp(NumericMatrix matrizDistancia,NumericVector consumo,int N, int raio){
  
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
NumericVector monteCarloSimulRadiusRcpp(NumericMatrix matrizDistancia,NumericVector consumo,int N, int raio, int bound){
  
  NumericVector resultSimul(bound);
  
  for(int i = 0; i < bound; i++){
    
    NumericVector dados = clone(consumo);
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::shuffle(dados.begin(),dados.end(),std::default_random_engine(seed));
    
    resultSimul(i) = clusterGeneratorRadiusSimulRcpp(matrizDistancia,dados,N,raio);
  }
  
  return(resultSimul);
}

// [[Rcpp::export]]
NumericMatrix createIndexMatrixRcpp(NumericMatrix matrizDistancia,int N) {
  
  NumericMatrix matIDX(N,N);
  NumericVector aux (N);
  NumericVector idx(N);
  
  for (int i = 0; i < N; i++){
    
    for (int h = 0; h < N; h++){ // copiando a linha correspondente da matriz de distancia
      aux(h) = matrizDistancia(i,h);
    }
    
    int x=0;
    std::iota(idx.begin(),idx.end(),x++);
    std::sort(idx.begin(),idx.end(),[&](int l,int m){return aux[l]<aux[m];});
    
    for (int h = 0; h < N; h++){
      matIDX(i,h) = idx(h);
    }
    
  }
  return (matIDX);
}

// [[Rcpp::export]]
NumericMatrix clusterGeneratorK_Rcpp(NumericMatrix IndexMatrix, NumericVector consumo,int N, int k){
  
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
    
    for(int j = 0; j < N; j++){
      if(j<k){ // os k primeiros vizinhos
        consumoIn = consumoIn + consumo(IndexMatrix(i,j));
        numIn = numIn + 1;
      }
      else{ // os demais alimentadores
        consumoOut = consumoOut + consumo(IndexMatrix(i,j));
        numOut = numOut + 1;
      }
    }
    
    miz = consumoIn/numIn;
    lambdaz = consumoOut/numOut;
    
    for(int j = 0; j < N; j++){
      if(j<k){ // os k primeiros vizinhos
        parteIn = parteIn + pow((consumo(IndexMatrix(i,j)) - miz),2);
      }
      else{ // os demais alimentadores
        parteOut = parteOut + pow((consumo(IndexMatrix(i,j)) - lambdaz),2);
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
double clusterGeneratorKSimulRcpp(NumericMatrix IndexMatrix,NumericVector consumo,int N, int k){
  
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
    
    for(int j = 0; j < N; j++){
      if(j<k){ // os k primeiros vizinhos
        consumoIn = consumoIn + consumo(IndexMatrix(i,j));
        numIn = numIn + 1;
      }
      else{ // os demais alimentadores
        consumoOut = consumoOut + consumo(IndexMatrix(i,j));
        numOut = numOut + 1;
      }
    }
    
    miz = consumoIn/numIn;
    lambdaz = consumoOut/numOut;
    
    for(int j = 0; j < N; j++){
      if(j<k){ // os k primeiros vizinhos
        parteIn = parteIn + pow((consumo(IndexMatrix(i,j)) - miz),2);
      }
      else{ // os demais alimentadores
        parteOut = parteOut + pow((consumo(IndexMatrix(i,j)) - lambdaz),2);
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
NumericVector monteCarloSimulK_Rcpp(NumericMatrix IndexMatrix,NumericVector consumo,int N, int k, int bound){
  
  NumericVector resultSimul(bound);
  
  for(int i = 0; i < bound; i++){
    
    NumericVector dados = clone(consumo);
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::shuffle(dados.begin(),dados.end(),std::default_random_engine(seed));
    
    resultSimul(i) = clusterGeneratorKSimulRcpp(IndexMatrix,dados,N,k);
  }
  
  return(resultSimul);
}