## This is programming assignment # 2 for "R programming" language course.
## The file contains 2 functions as described in detail below
## The purpose of assignment seems like to teach concept of 
## function factory and caching.

## Function "makeCacheMatrix" defines and stores following functions:
## setM - caches the supplied matrix and resets "inverseMatrix" to NULL
##             as it needs to be recalculated and cached
## getM - returns cached matrix
## setInvM - caches the "inverse" matrix 
## getInvM - returns "inversed"cached matrix

makeCacheMatrix <- function(x = matrix()) { 
  invM <- NULL    ## initialize variable where inverse matrix will be stored
  setM <- function(y) { ## this function stores the original matrix
    x <<- y             ## permanantly store suppied matrix y in x using global assignment operator
    invM <<- NULL       ## initialize variable where inverse matrix will be stored
  }
  getM <- function() x  ## get value of original matrix
  setInvM <- function(invMatrix) invM <<- invMatrix ## set value of "inverse" matrix
  getInvM <- function() invM ## get value of "inverse" matrix
  list(setM = setM, getM = getM, 
       setInvM = setInvM,
       getInvM = getInvM)
}

## The function cacheSolve 

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  invM <- x$getInvM()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$getM()
  invM <- solve(data)
  x$setInvM(invM)
  invM
} 

