library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

source("3H code files/Ensemble Creation.R")

#This was built to work for ensemble size 5, if more added, then add 
#terms to the function fcnPyx

EnsembleInterpretation<-function(Data,N,x,sigma){
  #Extract the ensemble High values of the data set here
#  EnsembleCreation(Data,N,x)
  EnsembleHighValues<-EnsembleCreation(Data,N,x)

  #Creation of the equation for the probability distribution
  fcnPyx<-function(y){(1/N*sigma)*((1/sqrt(2*pi))*
                    exp((-1/2)*((y-EnsembleHighValues[1])/sigma)^2)+
                    1/sqrt(2*pi)*
    exp((-1/2)*((y-EnsembleHighValues[2])/sigma)^2)+
      1/sqrt(2*pi)*exp((-1/2)*((y-EnsembleHighValues[3])/sigma)^2)+
      1/sqrt(2*pi)*exp((-1/2)*((y-EnsembleHighValues[4])/sigma)^2)+
      1/sqrt(2*pi)*
      exp((-1/2)*((y-EnsembleHighValues[5])/sigma)^2))}
  
  plot(fcnPyx(400:700),type='l')
}

#Plots to use: x=3 and (450:600)
#x=914 and (450:600)
#x=1156 and (450:600)
#this is for the given sigma = 4 