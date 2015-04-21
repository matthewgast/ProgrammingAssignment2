## cacheMatrix.R
##
## Matthew Gast, 2015
##
## These functions replace standard matrix functions and
## cache the inverse of a matrix rather than compute it every
## time.

makeCacheMatrix <- function(x = matrix()) {
## This function creates an object that caches a matrix inverse.
##
## Input:  A matrix object
## Output: A list containing the matrix and a cached inverse.  If
##         the matrix is not invertible, the cache is NULL.
  
  inv <- NULL
  
  setmtx <- function (mtx) {
    x <<- mtx     # Set matrix value
    inv <<- NULL  # Clear the cache on setting value
  }
  getmtx <- function () x
  setsolve <- function (solution) inv <<- solution
  getsolve <- function () inv
  
  # Construct return vector
  list (setmtx=setmtx, getmtx=getmtx,
        setsolve=setsolve,
        getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if (!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  }
  mtx <- x$getmtx
  inv <- solve(mtx)
  x$setsolve(inv)
  inv
}
