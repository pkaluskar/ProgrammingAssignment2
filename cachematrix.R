## makeCacheMatrix() and cacheSolve() are routines that optimize
## matrix operations by caching matrix-inversions.

## makeCacheMatrix() creates an "object" that holds two variables
## (the original "origMatrix" and its "inverse").
## It also proivides 4 methods - set/getmatrix() and set/getcache()
## to operate on these variables.
##
## cacheSolve() is called with the makeCacheMatrix-type matrix (mx);
## and returns the inverse (either from the cache or computed)
## 

makeCacheMatrix <- function(x = matrix()) {
  ## Data Objects
  inverse <- NULL
  
## Functions / Methods
  setcache <- function(value) {
    inverse <<- value
  }
  getcache <- function() {
    inverse
  }
  list(setcache = setcache,   getcache = getcache)
}


## Write a short comment describing this function

cacheSolve <- function(mx, ...) {
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
