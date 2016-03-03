##-----------------------------------------------------------------------------
## R functions that cache the inverse of a matrix. This functionality is 
## achived by the use of closures and the double arrow assignment operator <<-
## This is the homework assigment for Week 3 of Coursera's R Programming
##-----------------------------------------------------------------------------

## Returns a List of functions that are capable of caching a matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns an inverse matrix of x.  If a cached version of the computed 
## inverse of x is found, return it.  If not, compute the inverse of x, 
## cache it and return the inverse of x to the caller

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(a = data, ...)
  x$setinverse(i)
  i
}
