library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

source("3H code files/Ensemble Creation.R")

#EnsembleEvaluation(BAEData,5,514,4)

EnsembleEvaluation<-function(Data,N,x,sigma){
  #Calculate p(Y) such that Y is the verification of the image of High value of 
  #entry x
  #Ignorance Score is being used, hence -log(p(Y))
  EnsembleHighValues<-EnsembleCreation(Data,N,x)
  print(EnsembleHighValues)
  #High value of entry x and image x+1
  HighValue<-na.omit(as.numeric(as.character(Data$High)))
  HighValuex<-HighValue[x]
  ImageHighValuex<-HighValue[x+1]
#  print(ImageHighValuex)

  #Empty data frame for the eta values - etaValues
  etaValues<-as.matrix(rep(0,N))
  
  #Calculation of the eta values of verification - ensemble high prediction / sigma
  for (i in 1:N){
    etaValues[i]<-(ImageHighValuex-EnsembleHighValues[i])/sigma
  }
  #Empty data frame for the Gaussian Kernel K(eta) - Keta
  Keta<-as.matrix(rep(0,N))
  
  #Calculation of the K(eta) values - Gaussian K and eta
  for (i in 1:N){
    Keta[i]<-(1/sqrt(2*pi))*exp((-1/2)*etaValues[i]^2)
  }
#  print(Keta)
  
  #Sum of the Keta values - SumKeta
  SumKeta<-sum(Keta)
#  print(SumKeta)
  
  #Gaussian kernel distribution with verification Y - pY
  pY<-(1/N*sigma)*SumKeta
#  print(pY)
  
  #Ignorance Score calculation - IgnoranceScore
  IgnoranceScore<-(-log(pY))
  print(IgnoranceScore)
}
