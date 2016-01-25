#Problem 6 Solution and Plots for Homework 1 BNFO 285
#Kanishk Asthana kasthana@eng.ucsd.edu
library("MASS")
data=read.csv("hw1-prob6-data.csv", header=FALSE)
colnames(data)<-c("x","y","t")
timepoints=unique(data$t)

##############################################################################
#Part (a)

coordinatesAtEachTimePoint=lapply(timepoints,function(time){
  timePointData=data[data$t==time,c(1,2)]
  return(timePointData)
})

sampleMeansAtEachTimePoint=sapply(timepoints,function(time){
  #Getting coordinates for each timepoint
  timePointData=data[data$t==time,c(1,2)]
  return(c(mean(timePointData[,1]),mean(timePointData[,2])))
})
sampleMeansAtEachTimePoint=t(sampleMeansAtEachTimePoint)

plot(sampleMeansAtEachTimePoint,xlab="X",ylab="Y",main="Scatter plot of sample means")

sampleCovarianceMatrixAtEachTimePoint=lapply(timepoints,function(time){
  #Getting coordinates for each timepoint
  timePointData=data[data$t==time,c(1,2)]
  return(cov(timePointData[,c(1,2)]))
})

#############################################################################
#Part (b)

LogLikelihoodsAtEachTimePoint=sapply(1:length(sampleCovarianceMatrixAtEachTimePoint),function(n){
  covMatrix=sampleCovarianceMatrixAtEachTimePoint[n][[1]]
  mean=t(as.matrix(sampleMeansAtEachTimePoint[n,]))
  values=as.matrix(coordinatesAtEachTimePoint[n][[1]])
  inverseCovarianceMatrix=ginv(covMatrix)
  LogLikeLihoods=sapply(1:nrow(values),function(i){
    value=values[i,1]
    LogLikeLihood=(-1/2)*log(det(covMatrix)) - log(2*pi) -(1/2)*((value-mean)%*%(ginv(covMatrix)%*%t(value-mean)))
  })
  return(sum(LogLikeLihoods)) 
  
})


################################################################################
#Part(c)

sumofDiagnalElements=sapply(1:length(sampleCovarianceMatrixAtEachTimePoint),function(n){
  covMatrix=sampleCovarianceMatrixAtEachTimePoint[n][[1]]
  return(sum(diag(covMatrix)))
})
pdf("PlotforProblem6.pdf")
plot(LogLikelihoodsAtEachTimePoint,sumofDiagnalElements,main="Plot of LogLikelihoodsAtEachTimePoint vs Sum of Diagonal Elements\n of covariance Matrix at each time point")
dev.off()