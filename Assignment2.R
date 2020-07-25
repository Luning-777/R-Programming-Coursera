# This is the coding for assignment 2

pollutantmean <- function(directory,pollutant,id=1:332){
  data<-data.frame()
  for (i in 1:length(id)){
    files<-paste(directory,'/', formatC(id[i], width=3, flag="0"),'.csv', sep="")
    newdata<-read.csv(files,header=TRUE)
    #View(newdata)
    data<- rbind(data,newdata)
    #View(data)
  }
  pol_mean<-mean(data[[pollutant]],na.rm=TRUE)
  pol_mean
}


complete <- function(directory,id=1:332){
  data<-data.frame()
  for(i in 1:length(id)){
    files<-paste(directory,'/', formatC(id[i], width=3, flag="0"),'.csv', sep="")
    newdata<-read.csv(files)
    newline<-data.frame(id=id[i],nobs=sum(complete.cases(newdata)))
    data<-rbind(data,newline)
  }
  data
}


corr<-function(directory,threshold=0){
  correlation<-c()
  cases<-complete(directory)
  id<-cases[cases$nobs>threshold,]$id
  data<-data.frame()
  for(i in 1:length(id)){
    files<-paste(directory,'/', formatC(id[i], width=3, flag="0"),'.csv', sep="")
    newdata<-read.csv(files)
    newdata<-newdata[complete.cases(newdata),]
    correlation[i]<-cor(newdata$sulfate,newdata$nitrate)
    
  }
  correlation
}
