corr <- function(directory, threshold=0){
  ##' 'directory' is a character vector of length 1 indicating
  ##' the location of the CSV files
  ##' 
  ##' 'thershold' is a numberic vector of length 1 indicating the
  ##' number of completely observed observations (on all
  ##' variables) required to compute the correlation between
  ##' nitrate and sulfate; the default is 0
  ##' 
  ##' Return a numeric vector of correlations
  ##' NOTE: Do not round the result!
  result <- c()
  id<-1:332
  for(num in id){
    zeroes <- ""
    if(num < 10){
      zeroes<-"00"
    }
    else if(num >= 10 & num < 100){
      zeroes<-"0"
    }
    good <- complete.cases(read.csv(file=paste(getwd(),'/',directory,'/',zeroes,num,".csv",sep=""),header=TRUE, sep=","))
    myFile <- read.csv(file=paste(getwd(),'/',directory,'/',zeroes,num,".csv",sep=""),header=TRUE, sep=",")
    nubs<-sum(good)
    if(nubs > threshold){
      result<- c(result,cor(myFile[['sulfate']][good],myFile[['nitrate']][good]))
    }
  }
  result
}