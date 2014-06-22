## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts the values and creates a matrix from them. Please note,
## the matrix has to be an Identity Matrix (all diagonal elements 1) to calculate
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                       ##initializing the matrix as NULL
  setmatrix <- function(y)  {                           ##defining the set function
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x                             ##returns x
  setinverse <- function(solve) m <<- solve       ##solve is used to calculate inverse
  getinverse <- function() m                      ##returns m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,        ##list of four functions
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse is already calculated, cacheSolve fetches the inverse stored in
##the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {             ##Checking if inverse is already calculated
    message("fetching the cached data")
    return(m)
  }
  data <- x$getmat()
  m <- solve(data, ...)       ##specific command to calculate inverse if the matrix     
  x$setinverse(m)
  m                           ## Returns a matrix that is the inverse of 'x'
}