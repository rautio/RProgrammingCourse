best<-function(state, outcome){
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

  ## Return hospital name in the state with lowest 30-day death
  ## rate
  data[23]<-suppressWarnings(as.numeric(data[,23]))
  data[11]<-suppressWarnings(as.numeric(data[,11]))
  data[17]<-suppressWarnings(as.numeric(data[,17]))
  data<-data[data[7]==state,]
  hospitals <- suppressWarnings(data[2][data[col] == min(data[,col],na.rm=TRUE)])
  min(hospitals,na.rm=TRUE)[1]
}