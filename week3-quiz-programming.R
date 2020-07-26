library('datasets')
data(iris)
?iris
#calculate the mean value of sepal.length of each species of iris
tapply(iris[,1],iris$Species,mean)

library('mtcars')
data(mtcars)
?mtcars

#calculate the mean value of mpg in each category of cylinder numbers
with(mtcars,tapply(mpg,cyl,mean))
tapply(mtcars$mpg,mtcars$cyl,mean)
sapply(split(mtcars$mpg,mtcars$cyl),mean)


#find the absolute difference in average horsepower of cars with 4 cylinders and average horsepower of cars with 8 cylinders.
horsepower<-tapply(mtcars$hp,mtcars$cyl,mean)
abs(horsepower['4']-horsepower['8'])