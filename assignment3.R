## Here I write two functions. The first one creates a Matrix object, and the second one compute its inverse.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){inv <<- inverse}
  getinverse <- function() inv
  list(set=set,get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This function calculates the inverse of the special 'matrix' created in makeCacheMatrix
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message(('getting cached data'))
    return(inv)
  }
  mt <- x$get()
  inv <- solve(mt)
  x$setinverse(inv)
  inv
}

