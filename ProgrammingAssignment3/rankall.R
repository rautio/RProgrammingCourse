rankall <- function(outcome, num="best"){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  col<-FALSE
  switch(outcome,
         'heart failure'={col<-17},
         'heart attack'={col<-11},
         'pneumonia'={col<-23},
         FALSE)
  if(!col) stop("Invalid outcome")

  ## For each state, find the hospital of the given rank
  data[,23]<-suppressWarnings(as.numeric(data[,23]))
  data[,11]<-suppressWarnings(as.numeric(data[,11]))
  data[,17]<-suppressWarnings(as.numeric(data[,17]))
  
  result<-by(data, data[,7],function(x){
    x$rank <-as.numeric(factor(x[,col]))
    if(num == "best"){
      num <- min(x$rank,na.rm=TRUE)
    }
    if(num == "worst"){
      num <- max(x$rank,na.rm=TRUE)
    }
    hospitals <- x[,2][x$rank == num]
    hospitals <- hospitals[!is.na(hospitals)]
    hospitals <- sort(hospitals)
    hospital <- hospitals[1]
    st <- min(x[,7],na.rm=TRUE)
    return(c(hospital,st))
  })
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result<-do.call(rbind,result)
  result<-as.data.frame(result)
  colnames(result)<-c("hospital","state")
  result
}