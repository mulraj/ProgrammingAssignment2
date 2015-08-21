## This is programming assignment#2 for "R programming" language course.
## The file contains 2 functions as described in detail below.
## The purpose of assignment seems like to teach concept of 
## lexical scoping, function factory, caching to improve performance.

## (Following note is for my own reference:
## Ideally inverse of matrix should be computed in makeCacheMatrix 
## function itself. Current solution modeled after makeVector example
## can lead to data integrity issue as the a random value can be set
## for "inverse" matrix by directly calling setInv with any matrix
## as parameter. (makeVector examle has similar data integrity problem, and 
## mean should be calculated and stored as part of makeVector function.))

## Function "makeCacheMatrix" defines and stores following functions:
## setM - caches the supplied matrix and resets "inverseMatrix" to NULL
##             as it needs to be recalculated and cached
## getM - returns cached matrix
## setInvM - caches the "inverse" matrix 
## getInvM - returns "inversed"cached matrix

## (note to fellow student evaluating the course. At the end of file I
## have included one of the test cases used to test these functions)

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

## The function "cacheSolve" checks if "inverse" matrix is already 
## computed and stored - if so it returns the value from cache
## if not than it computes the value, calls set function to cache it
## and returns the value.

cacheSolve <- function(x, ...) { 
  invM <- x$getInvM()   ## try to get "inverse" matrix
  if(!is.null(invM)) {  ## if "inverse" matrix is not NULL
    message("getting cached data")
    return(invM)        ## return "inverse" matrix
  }
  ## if "inverse" matrix doesn't exist (is NULL)
  data <- x$getM()       ## get original matrix
  invM <- solve(data)    ## compute "inverse" of original matrix
  x$setInvM(invM)        ## cache "inverse"of original matrix
  invM                   ## Return "inverse" matrix
} 

## Test run "pasted from "R console"
## > a <- matrix(c(0, 7, -1, 0), 2, 2) ## created a test matrix
## > a                                 ## printed the test matrix
##      [,1] [,2]
## [1,]    0   -1
## [2,]    7    0
## > solve(a) ## printed "inverse" matrix just to compare results returned by the function later
##      [,1]      [,2]
## [1,]    0 0.1428571
## [2,]   -1 0.0000000
## > m <- makeCacheMatrix (a) ## executed makeCacheMatrix
## > m$getM() ## note that getM function returns original matrix accurately
##      [,1] [,2]
## [1,]    0   -1
## [2,]    7    0
## > cacheSolve(m) ## note that cacheSolve returns "inverse" value accurately
##      [,1]      [,2]
## [1,]    0 0.1428571
## [2,]   -1 0.0000000
## > cacheSolve(m) ## note that second call to cacheSolve returns cached "inverse" value accurately
## getting cached data
##      [,1]      [,2]
## [1,]    0 0.1428571
## [2,]   -1 0.0000000
