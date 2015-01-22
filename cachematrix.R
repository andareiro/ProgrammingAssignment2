## These two functions invert a matrix and store a cached copy of that
## inverted matrix, returning the cached copy if the invert calculation
## has already been done.


## This function creates a list of four functions for storing a matrix
## and for caching the inverted copy of that matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Since we're calling this function, reset the inverted matrix variable
  mInverted <- NULL
  
  ## When setting an original matrix, reset the inverted matrix
  mSet <- function(y) {
    x <<- y
    mInverted <<- NULL
  }
  
  ## Simple function to retrieve the original matrix 
  mGet <- function() x
  
  ## Function to cache the inverted matrix
  mSetInvert <- function(solve) mInverted <<- solve
  
  ## Function to retrieve the inverted matrix
  mGetInvert <- function() mInverted
  
  ## Produce a list of all four functions
  list ( mSet = mSet, mGet = mGet, mSetInvert = mSetInvert, mGetInvert = mGetInvert )
}


## This function calculates and returns an inverted matrix,
## or returns a cached copy if the function has been run previously
## for the same matrix.

cacheSolve <- function(x, ...) {
  
  ## Try to retrieve the cached inverted matrix 
  m <- x$mGetInvert()
  
  ## Check to see if the cache matrix is there - if so, output that matrix
  if(!is.null(m)) {
    message("Gettin' me some cached data")
    return(m)
  }
  
  ## No cached matrix? Durn. Get the original...
  data <- x$mGet()
  
  ## ...invert the new matrix...
  m <- solve(data, ...)
  
  ## ...store the inverted matrix in the cache...
  x$mSetInvert( m )
  
  ## ...and finally give the inverted matrix to the user as output!
  ## Boom.
  m
}