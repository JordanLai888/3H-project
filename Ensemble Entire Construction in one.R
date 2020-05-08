library(readr)
BAEData<-read.csv("Financial Data/BA.LMax.csv")

source("3H code files/Ensemble Creation.R")
source("3H code files/Ensemble Interpretation.R")
source("3H code files/Ensemble Evaluation.R")


EnsembleEntireConstruction<-function(Data,N,x,sigma){
  EnsembleInterpretation(Data,N,x,sigma)
  EnsembleEvaluation(Data,N,x,sigma)
}

Date<-c("27/04/2015", "17/07/2017", "02/04/2020")
IgnScorePredModel<-c(-0.282621, -1.043392,  -0.9104577  )
IgnScoreUnconGauss<-c(2.586092981, 2.218979775, 18.53058493)
cbind(Date,IgnScorePredModel,IgnScoreUnconGauss)
