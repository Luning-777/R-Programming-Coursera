# this is for assignment4 in week 4
## Question 1

## read data from the given file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

nrow(outcome)
ncol(outcome)
colnames(outcome)


## make a simple histgram of the recent 30 days deaths
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


## write a function that n reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. 

## The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of heart attack, heart failure, or pneumonia. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings

best <- function(state,outcome){
  ## read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## check that the state and outcome is valid
  disease <-c('heart attack', 'heart failure','pneumonia')
  if(any(outcome_df[,7] == state) & any(disease==outcome)){
    ## create a data frame by hospitals within this state
    df_2<-outcome_df[outcome_df[,7]==state,]
    ##find the column of outcome
    columns<- c(11,17,23)[disease==outcome]
    ## order our data frame by ascending death rate of our outcome and then by ascending alphabetic hospital names
    df_order<- df_2[order(as.numeric(df_2[,columns]),df_2$Hospital.Name),]
    ## return the name of best hospital
    hospital <- df_order[1,'Hospital.Name']
  }
  else if(! any(outcome_df[,7]== state)){hospital <-'invalid state'}
  else {hospital <-'invalid outcome'}
  return(hospital)
}

## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  disease <-c('heart attack', 'heart failure','pneumonia')
  if(any(outcome_df[,7] == state) & any(disease==outcome)){
    ## create a data frame by hospitals within this state
    df_2<-outcome_df[outcome_df[,7]==state,]
    ##find the column of outcome
    columns<- c(11,17,23)[disease==outcome]
    ## check if the ordering is decreasing or not
    if(num=='best'){
      ## order our data frame by ascending death rate of our outcome and then by ascending alphabetic hospital names
      df_order<- df_2[order(as.numeric(df_2[,columns]),df_2$Hospital.Name),]
    } else if (num=='worst'){
      ## order our data frame by ascending death rate of our outcome and then by ascending alphabetic hospital names
      df_order<- df_2[order(-as.numeric(df_2[,columns]),df_2$Hospital.Name),]
    } else if (is.numeric(num)){
      ## order our data frame by ascending death rate of our outcome and then by ascending alphabetic hospital names
      df_order<- df_2[order(as.numeric(df_2[,columns]),df_2$Hospital.Name),]
      df_order<-df_order[num,]
    }
    ## return the name of best hospital
    hospital <- df_order[1,'Hospital.Name']
  } else if(! any(outcome_df[,7]== state)){
    hospital<-'invalid state'} else {
      hospital<-'invalid outcome'}
  ## Return hospital name in that state with the given rank 30-day death rate
  hospital
}


## a function that gives the hospital name has the given rank among death rate in outcome
rankall<- function(outcome,num='best'){
  outcome_df<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## check if the outcome is valid
  disease <-c('heart attack','heart failure','pneumonia')
  if(any(disease==outcome)){
    columns<- c(11,17,23)[disease==outcome]
    hospital<-c()
    state<-c()
    
    if(num=='best'){
      for (state1 in unique(outcome_df[,7])) {
        df2<-outcome_df[outcome_df[,7]==state1,]
        orderdf<-df2[order(as.numeric(df2[,columns]),df2[,2]),]
        hospital[state1]<-orderdf[1,'Hospital.Name']
        state[state1]<-state1
      }
    } else if(num=='worst'){
      for (state1 in unique(outcome_df[,7])) {
        df2<-outcome_df[outcome_df[,7]==state1,]
        orderdf<-df2[order(-as.numeric(df2[,columns]),df2[,2]),]
        hospital[state1]<-orderdf[1,'Hospital.Name']
        state[state1]<-state1
        
      }
    } else if (is.numeric(num)){
      for (state1 in unique(outcome_df[,7])) {
        df2<-outcome_df[outcome_df[,7]==state1,]
        orderdf<-df2[order(as.numeric(df2[,columns]),df2[,2]),]
        hospital[state1]<-orderdf[num,'Hospital.Name']
        state[state1]<-state1
        
      }
    }
    hospitaldf<-data.frame(hospital,state)
    hospitaldf<-hospitaldf[order(hospitaldf$state),]
  } else{hospitaldf<-'invalid outcome'}
  hospitaldf
}
