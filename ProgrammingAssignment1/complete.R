complete<-function(directory, id=1:332){
  ##' 'directory' is a character vector of length 1 indicating
  ##' the locaiton of the CSV files.
  ##' 'id' is an integer vcetor indicating the monitor ID numbers
  ##' to be used.
  ##' Return a data frame of the form:
  ##' id nobs
  ##' 1 117
  ##' 2 1041
  ##' ...
  ##' where 'id' is the monitor ID number and 'nobs' is the 
  ##' number of complete cases
  ids <- c()
  nobs <- c()
  for( num in id){
    zeroes <- ""
    if(num < 10){
      zeroes<-"00"
    }
    else if(num >= 10 & num < 100){
      zeroes<-"0"
    }
    myFile <- complete.cases(read.csv(file=paste(getwd(),'/',directory,'/',zeroes,num,".csv",sep=""),header=TRUE, sep=","))
    ids<-c(ids,num)
    nobs<-c(nobs,sum(myFile))
  }
  result <- data.frame(id,nobs)
  result
}
