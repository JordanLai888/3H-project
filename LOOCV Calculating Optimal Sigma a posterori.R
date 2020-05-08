library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

source("3H code files/Ensemble Creation.R")
source("3H code files/Ensemble Interpretation.R")
source("3H code files/Ensemble Evaluation.R")

LOOCVOptimalSigmaValue(BAEData,5)

LOOCVOptimalSigmaValue<-function(Data,N){
  SigmaTestList<-seq(0, 5, by=0.5)
  HighValue<-na.omit(as.numeric(as.character(Data$High)))
  NumRows<-length(HighValue)-1
  NumberSigmaTest<-length(SigmaTestList)
  LOOCVEmpericalIgnoranceSet<-as.matrix(rep(0,NumberSigmaTest))
  
  LOOCV<-sample(x=Data, size=NumRows, replace=TRUE)

  for (i in 1:NumberSigmaTest){
    IgnoranceValueEachEntry<-as.matrix(rep(0,NumRows))
    for (j in 1:NumRows){
      IgnoranceValueEachEntry[j]<-EnsembleEvaluation(LOOCV,5,j,SigmaTestList[i])
      
    }
    LOOCVEmpericalIgnoranceSet[i]<-(1/NumRows)*sum(IgnoranceValueEachEntry)
  }
#  print(LOOCVEmpericalIgnoranceSet)
  
  #Index of the sigma value that will gives the smallest emperical ignorance value
  IndexSigma<-head(order(LOOCVEmpericalIgnoranceSet,decreasing = FALSE),n=1)
  
  #Determining the optimal sigma value from the list with the index for the optimal
  OptimalSigma<-SigmaTestList[IndexSigma]
  print(OptimalSigma)  
}
#The LOOCV optimal sigma calculated a posterori is 5 
