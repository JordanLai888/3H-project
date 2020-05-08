library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

###Denote: N - The number of ensemble members used in the forecast
### x - The entry for which the image is to predicted
EnsembleCreation(BAEData,5,2)
EnsembleCreation<-function(Data,N,x){
  #Values extracted from data 
  HighValue<-na.omit(as.numeric(as.character(Data$High)))
  LowValue<-na.omit(as.numeric(as.character(Data$Low)))
  OpenValue<-na.omit(as.numeric(as.character(Data$Open)))
  CloseValue<-na.omit(as.numeric(as.character(Data$Close)))
  
  #Data frame containing the H, L, O, C values of the data - CompleteDataFrame
  CompleteDataFrame<-as.matrix(cbind(HighValue, LowValue, OpenValue, CloseValue))
#  print(head(CompleteDataFrame))

  #Difference values between H-L, O-C for each data entry 
  DiffHLDataSet<-CompleteDataFrame[,1]-CompleteDataFrame[,2]
  DiffOCDataSet<-CompleteDataFrame[,3]-CompleteDataFrame[,4]
    
  #Data frame containing the difference in H-L and O-C of the data - DiffDataFrame
  DiffDataFrame<-as.matrix(cbind(DiffHLDataSet,DiffOCDataSet))
#  print(head(DiffDataFrame))
  
  #The H, H-L, O-C values for entry of interest x - HighValuex, DiffHLValuex, 
                                                                #DiffOCValuex
  HighValuex<-HighValue[x]
  DiffHLValuex<-DiffDataFrame[x,1]
  DiffOCValuex<-DiffDataFrame[x,2]
#  print(HighValuex)
#  print(DiffHLValuex)
#  print(DiffOCValuex)
  
  #Number of rows in data frame
  NumRows<-length(HighValue)-1
#  print(NumRows)
  
  #Empty data frame ready for the Difference metrics of x and all other entries
  #The first column gives difference in HL difference metric
  #The second column gives difference in OC difference metric 
  DiffxAndOther<-as.matrix(cbind(rep(0,NumRows),rep(0,NumRows)))
  
  #Calculation of the absolute difference between difference metrics between
                                                    #x and all other entries
  for (i in 1:NumRows){
    DiffxAndOther[i,1]<-abs(DiffDataFrame[i,1]-DiffHLValuex)
    DiffxAndOther[i,2]<-abs(DiffDataFrame[i,2]-DiffOCValuex)
  }
#  print(head(DiffxAndOther))
  
  #######
  #DiffxAndOther data frame omitting entry x 
  DiffxAndOtherOmittedx<-DiffxAndOther[-(x),]
#  print(head(DiffxAndOtherOmittedx))

  #min, max of the differences of each difference metrix of DiffxAndOtherOmittedx
  #minDiffxAndOtherHL, maxDiffxAndOtherHL, minDiffxAndOtherOC, maxDiffxAndOtheroc
  minDiffxAndOtherHL<-min(DiffxAndOtherOmittedx)
  maxDiffxAndOtherHL<-max(DiffxAndOtherOmittedx)
  minDiffxAndOtherOC<-min(DiffxAndOtherOmittedx)
  maxDiffxAndOtherOC<-max(DiffxAndOtherOmittedx)
  
  #Normalised absolute difference between differnece metrics between x and all
  #other entries, empty data frame - DiffxAndOtherNormalised
  DiffxAndOtherNormalised<-as.matrix(cbind(rep(0,NumRows),rep(0,NumRows)))
  
  #Calculation to find the normalised difference metrics 
  for (i in 1:NumRows){
    DiffxAndOtherNormalised[i,1]<-(DiffxAndOther[i,1]-minDiffxAndOtherHL)/(maxDiffxAndOtherHL-minDiffxAndOtherHL)
    DiffxAndOtherNormalised[i,2]<-(DiffxAndOther[i,2]-minDiffxAndOtherOC)/(maxDiffxAndOtherOC-minDiffxAndOtherOC)
  }
  
#####  
  #Empty data frame ready for the sum of the differences metrics - SumDiffs
  SumDiffs<-as.matrix(rep(0,NumRows))
#  print(SumDiffs)
  
  #Find the sum of the difference between entry x and the other entries 
  for (i in 1:NumRows){
    SumDiffs[i]<-DiffxAndOtherNormalised[i,1]+DiffxAndOtherNormalised[i,2]
  }
#  print(head(SumDiffs))
  
  #Finding the index of the N nearest neighbours by ordering the SumDiffs -
                                                              #SumDiffIndex
  SumDiffIndex<-head(order(SumDiffs,decreasing = FALSE),n=N+1)
  
  #Index of the N nearest neighbours omitting the first index as it is 0, since
  #the entry with the smallest SumDiff is itself - IndexOfNearestNeighbours
  IndexOfNearestNeighbours<-SumDiffIndex[-1]
#  print(SumDiffIndex)
#  print(IndexOfNearestNeighbours)

  #Empty Data frame for the difference between the high value of the nearest 
                      #neighbours and their respective image - HighDiffNN

  HighDiffNN<-as.matrix(rep(0,N))
  
  #Find the difference between the High value of the NN and their image
  for (i in 1:N){
    HighDiffNN[i]<-HighValue[IndexOfNearestNeighbours[i]+1]-HighValue[IndexOfNearestNeighbours[i]]
  }
#  print(HighDiffNN)
  
  #Empty Data frame to add the difference in High values between the nearest 
  #neighbours and their image, to the High value of entry x to create the 
  #ensemble of the predicted high values - HighValueEnsemble
  HighValueEnsemble<-as.matrix(rep(0,N))
  
  #Find the predicted high value of the x+1th entry using the difference 
  #in high value of the nearest neighbours 
  for (i in 1:N){
    HighValueEnsemble[i]<-HighValuex+HighDiffNN[i]
  }
  print(NumRows)
  
  #The final ensemble for the prediction of the x+1th day is given by
  #HighValueEnsemble
  return(HighValueEnsemble) #print this if you want the ensemble
  
}

####################



