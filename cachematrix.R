## cacheMatrix.R
##
## Matthew Gast, 2015
##
## These functions build on standard matrix functions by storing the
## inverse of a matrix in a list with the matrix itself.

makeCacheMatrix <- function(x = matrix()) {
## This function creates an object containing a matrix and its inverse.
##
## Input:  A matrix object.
## Output: A list containing the the matrix and its cached inverse.
## If the matrix is not invertible, the cache is NULL.
    
  inverse <- NULL

  # Function 1: set the matrix value (and clear the inverse cache)
  setMatrix <- function (mtx) {
    x <<- mtx
    inverse <<- NULL
  }

  # Function 2: return the matrix
  getMatrix <- function () x

  # Function 3: set the inverse value
  setInverse <- function (myInverse) inverse <<- myInverse

  # Function 4: return the inverse value
  getInverse <- function () inverse
  
  # Construct return vector
  list (setMatrix=setMatrix,
        getMatrix=getMatrix,
        setInverse=setInverse,
        getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
## This function returns the inverse of the matrix passed as the first object.
##
## Input:  A matrix object created by makeCacheMatrix
## Output: The function returns the inverse of the matrix.  It first
## checks the cache to see if the inverse exists, and if not, it
## calculates the inverse.

  # Retrieve cache value if it exists
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Retrieving cached inverse")
    return(inverse)
  }

  isInvertible <- function (m) {
    # This function tests whether a matrix is invertible, using two tests:
    # (1) if the determinant is zero
    # (2) if any element of the singular value decomposition is zero
    # 
    # Note: zero comparisons must be done against a small "epsilon"
    # defined by R for machine precision
    #
    # Test 1: test whether determinant is zero
    if (det(m) < .Machine$double.eps) {
      return(FALSE)
    }
    # Test 2: test whether any element in the SVD is zero
    if (any(svd(m)$d < .Machine$double.eps)) {
      return(FALSE)
    }
    # Otherwise, the matrix is invertible
    return(TRUE)
  }
  
  # Otherwise, calculate inverse and cache it
  mtx <- x$getMatrix()
  if (isInvertible(mtx)) {
      inverse <- solve(mtx)
  } else {
      message("Non-invertible matrix")
      inverse <- NULL
  }
  x$setInverse(inverse)
  inverse
}
