## Put comments here that give an overall description of what your
## functions do.
# in order to compute the inverse of the given matrix, following two functions are implemented.
# Since inverse calculation can be a compute-intensive process, it is better to cache the results for future usage.
# The following two functions help in achieving this functionality.

## Write a short comment describing this function.
# In this function following activities are performed.
# set<-function sets the value of matrix.
# get<-function returns the matrix.
# setinverse function is used to set the inverse of the matrix.
# getinverse function is used to return the value of matrix inverse previously set using setinverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get function
  get <- function() x
  
  #functions to set and get inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #exposed functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function.
# This function calculates and returns the inverse of matrix.
# In doing so, it first checks if the matrix inverse is already computed and is available in the cache.
# This check is performed using is.null(inv). If there is inverse available, it returns the inv variable.
# Otherwise, it gets the matrix, calculates the inverse and store it in cache using setinverse, and returns the result i.e. inv at the end.
cacheSolve <- function(x, ...) {
  
  #first check if inverse available
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("returning data from cache.")
    return(inv)
  }
  
  #calculate inverse
  data <- x$get()
  inv <- solve(data)
  
  #set the matrix inverse in cache
  x$setinverse(inv)
  
  #return inv value
  inv
}
