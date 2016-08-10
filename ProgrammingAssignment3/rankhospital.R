rankhospital <- function(state,outcome,num="best"){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  col<-FALSE
  switch(outcome,
         'heart failure'={col<-17},
         'heart attack'={col<-11},
         'pneumonia'={col<-23},
         FALSE)
  ## Check that state and outcome are valid
  if(!is.element(state,data[,7])) stop("Invalid state.")
  if(!col) stop("Invalid outcome")

  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data[,23]<-suppressWarnings(as.numeric(data[,23]))
  data[,11]<-suppressWarnings(as.numeric(data[,11]))
  data[,17]<-suppressWarnings(as.numeric(data[,17]))
  data<-data[data[7]==state,]
  data$rank <-as.numeric(factor(data[,col]))
  if(num == "best"){
    num <- min(data$rank,na.rm=TRUE)
  }
  if(num == "worst"){
    num <- max(data$rank,na.rm=TRUE)
  }
  hospitals<-data[2][data$rank==num,]
  min(hospitals,na.rm=TRUE)
}