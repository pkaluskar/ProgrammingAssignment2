## makeCacheMatrix() and cacheSolve() are routines that optimize
## matrix operations by caching matrix-inversions.

## makeCacheMatrix() creates an "object" that holds the "inverse" of the matrix
## It also proivides 3 methods - getmatrix() to extract the original
## matrix; and set/getcache() to get and set the cached-inverse of the matrix.
##

makeCacheMatrix <- function(x = matrix()) {
  ## Data Objects:  Saved Inverse
  inverse <- NULL
  
  ## Functions / Methods
  getmatrix <- function() {
    x
  }

  setcache <- function( value ) {
    inverse <<- value
  }

  getcache <- function() {
    inverse
  }
  
  list(getmatrix = getmatrix,
       setcache = setcache,
       getcache = getcache)
}


## cacheSolve() is called with the makeCacheMatrix-type matrix 
## For eg:  x < matrix(1:9, 3,3)
##  and mx <- makeCacheMatrix(x)]
## It returns the inverse of the matrix (from the cache - if available, or from
## the computation - if not)
## 

cacheSolve <- function(mx, ...) {
  ## Return a matrix that is the inverse of 'x'
  tmp <- mx$getcache()
  
  # If value is found in the cache, return it
  if( !is.null( tmp ) ) {
    return( tmp )
  }
  # Value not found in cache; compute inverse, save it to cache, 
  # and it return to caller
  tmp <- solve( mx$getmatrix() )
  mx$setcache( tmp )
  tmp
}
