## makeCacheMatrix() and cacheSolve() are routines that optimize
## matrix operations by caching matrix-inversions.

## makeCacheMatrix() creates an "object" that holds two variables
## (the original "origMatrix" and its "inverse").
## It also proivides 4 methods - set/getmatrix() and set/getcache()
## to operate on these variables.
##
## cacheSolve() is called with the makeCacheMatrix-type matrix (mx);
## some other routine will perform the conversion.
## 

makeCacheMatrix <- function(x = matrix()) {
  ## Data Objects
  origMatrix <- NULL
  inverse <- NULL
  
## Functions / Methods
  setmatrix <- function(value) {
    origMatrix <<- value
  }
  getmatrix <- function() {
    origMatrix
  }
  setcache <- function(value) {
    inverse <<- value
  }
  getcache <- function() {
    ## Check if the origMatrix has been modified;
    y <<- origMatrix
    if (identical(x, y)) 
      return (inverse)
    else
      return (NULL)
  }
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setcache = setcache,   getcache = getcache)
}


## Write a short comment describing this function

cacheSolve <- function(mx, ...) {
  # create a the right object-type
  #   mx <- makeCacheMatrix(x)
  #   mx$setmatrix(x)
  
  ## Return a matrix that is the inverse of 'x'
  tmp <- mx$getcache()
  # If value is found in the cache
  if(!is.null(tmp)) {
    return(inverse)
  }
  # Value not found in cache; compute
  tmp <- solve(x)
  mx$setcache(tmp)
  tmp
}
