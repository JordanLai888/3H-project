library(readr)
BAEData<-read.csv("Financial Data/BA.L.csv")

source("3H code files/Ensemble Creation.R")

IgnoranceComparisonModelvGaussian(BAEData,5,8)

IgnoranceComparisonModelvGaussian<-function(Data,N,sigma){
  HighValue<-na.omit(as.numeric(as.character(Data$High)))
  NumRows<-length(HighValue)-1
  DateValue<-as.character(Data$Date)
  IgnoranceScoresModel<-as.matrix(rep(0,NumRows))
  IgnoranceScoresGaussian<-as.matrix(rep(0,NumRows))
  
#for the model
  for (x in 1:NumRows){
    EnsembleHighValues<-EnsembleCreation(Data,N,x)
#    print(EnsembleHighValues)
    HighValue<-na.omit(as.numeric(as.character(Data$High)))
    HighValuex<-HighValue[x]
    ImageHighValuex<-HighValue[x+1]
    
    etaValues<-as.matrix(rep(0,N))
    for (i in 1:N){
      etaValues[i]<-(ImageHighValuex-EnsembleHighValues[i])/sigma
    }
    Keta<-as.matrix(rep(0,N))
    for (i in 1:N){
      Keta[i]<-(1/sqrt(2*pi))*exp((-1/2)*etaValues[i]^2)
    }
    SumKeta<-sum(Keta)
    #  print(SumKeta)
    
    pY<-(1/N*sigma)*SumKeta
    #  print(pY)
    
    IgnoranceScore<-(-log(pY))
    IgnoranceScoresModel[x]<-IgnoranceScore
  }

  #for the unconditional Gaussian
  for (x in 1:NumRows){
    Y<-HighValue[x+1]
    pY<-(1/(sqrt(4002.952*2*pi)))*exp((-(Y-559.4337)^2)/(2*4002.952))
    IgnoranceScore<-(-log(pY))
    IgnoranceScoresGaussian[x]<-IgnoranceScore
  }
  EnsembleVerificationpair<-c(1:100)
  
#  y1<-head(IgnoranceScoresModel,n=100)
#  y2<-head(IgnoranceScoresGaussian,n=100)
#  print(y1)
#  print(y2)
#  plot(EnsembleVerificationpair,y1,col="red",ylim = c(-4,7),type='l',xlab = "Ensemble-verification pair",ylab = "Ignorance Score")
#  lines(EnsembleVerificationpair,y2,col="Blue")
#  legend("bottomleft", legend=c("Share price prediction model", "Unconditional Gaussian Distribution"),
#         col=c("red", "blue"), lty=1:1, cex=0.8)
  
  
  IgnoranceDifference<-as.matrix(rep(0,NumRows))
  
  for (i in 1:NumRows){
    IgnoranceDifference[i]<-IgnoranceScoresModel[i]-IgnoranceScoresGaussian[i]
  }
  
  print(IgnoranceDifference)
  IgnoranceDiffereneceHead<-head(IgnoranceDifference,n=100)
  plot(EnsembleVerificationpair,IgnoranceDiffereneceHead,col="black",ylim = c(-8,1),type='l',xlab = "Ensemble-verification pair",ylab = "Difference of Ignorance Scores")
  meany<-mean(IgnoranceDifference)
  print(meany)
}
