source("3H code files/Ensemble Evaluation.R")
library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

source("3H code files/Ensemble Creation.R")

AverageEmpirical<-function(Data,N,sigma){
  HighValue<-na.omit(as.numeric(as.character(Data$High)))
  NumRows<-length(HighValue)
  
  EmpiricalScores<-as.matrix(cbind(rep(0,NumRows)))
  
  for (x in 1:NumRows){
    EmpiricalScoreRaw<-EnsembleEvaluation(Data,N,x,sigma)
    EmpiricalScores[x]<-EmpiricalScoreRaw
  }
  
  EmpiricalScores[!is.finite(EmpiricalScores)] <- NA
  colMeans(EmpiricalScores, na.rm=TRUE)
  
}
AverageEmpirical(BAEData,5,8)
