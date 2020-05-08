library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

source("3H code files/Ensemble Creation.R")
source("3H code files/Ensemble Interpretation.R")
source("3H code files/Ensemble Evaluation.R")

OptimalSigmaValue(BAEData,5)

OptimalSigmaValue<-function(Data,N){
  #Sigma value list to test - SigmaTestList
  SigmaTestList<-seq(0.5, 8, by=0.25)
#  print(SigmaTestList)
  
  #Number of rows in data frame
  HighValue<-na.omit(as.numeric(as.character(Data$High)))
  NumRows<-length(HighValue)-1
#  print(NumRows)

  #Number of rows in Sigma Value list 
  NumberSigmaTest<-length(SigmaTestList)
#  print(NumberSigmaTest)
  
  #Empty data frame for Emperical ignorance score - EmpericalIgnoranceSet
  EmpericalIgnoranceSet<-as.matrix(rep(0,NumberSigmaTest))
 
  #Testing the ignorance value for each sigma value
  for (i in 1:NumberSigmaTest){
    IgnoranceValueEachEntry<-as.matrix(rep(0,NumRows))
    for (j in 1:NumRows){
      IgnoranceValueEachEntry[j]<-EnsembleEvaluation(Data,5,j,SigmaTestList[i])
    }
    EmpericalIgnoranceSet[i]<-(1/NumRows)*sum(IgnoranceValueEachEntry)
  }
  print(EmpericalIgnoranceSet)
  
  #Index of the sigma value that will gives the smallest emperical ignorance value
  IndexSigma<-head(order(EmpericalIgnoranceSet,decreasing = FALSE),n=1)
  
  #Determining the optimal sigma value from the list with the index for the optimal
  OptimalSigma<-SigmaTestList[IndexSigma]
  print(OptimalSigma)
}  

  #The optimal sigma calculated a posterori is 5
  

