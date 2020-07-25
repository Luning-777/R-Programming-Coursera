library('datasets')
head(airquality)

#split the airquality data frame according to Months
s<-split(airquality,airquality$Month)
 
#calculate the mean of each columns within each month

#lapply will return a list
lapply(s, function(x) colMeans(x[,c('Ozone','Solar.R','Wind','Temp')]))

#sapply will return a matrix
sapply(s, function(x) colMeans(x[,c('Ozone','Solar.R','Wind','Temp')]))

#removing the na values by adding na.rm
sapply(s, function(x) colMeans(x[,c('Ozone','Solar.R','Wind','Temp')],na.rm=TRUE))


###to split a vector with more than one factors
x<-rnorm(10)
f1<-gl(2,5)
f2<-gl(5,2)
interaction(f1,f2) # this would create a new factor by f1 and f2 with 10 levels

split(x,list(f1,f2)) # split will call interaction function automatically
split(x,list(f1,f2),drop=TRUE)