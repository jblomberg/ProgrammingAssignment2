# Implementation of a CacheMatrix object, and a inverse solver
# function which supports caching of the result.

# Creates a wrapper object around a matrix, supporting
# the following functions:
# get() -> returns the original matrix
# set() -> sets a new value for the matrix, and clears any cached inverse
#          matrix
# setinverse() -> sets the inverse matrix
# getinverse() -> retrieves the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # set the initial value of the inverse matrix to null
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    # when setting a new value for the matrix, clear the inverse
    inverse <<- NULL
  }

  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# Accepts a CacheMatrix, and returns the inverse of its internal matrix.
# If the CacheMatrix has already cached an inverse, 
# it is returned instead of re-solving.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  # if there is a non-null inverse, we can just return it
  if(!is.null(inverse)) {
    return(inverse)
  }
  # fall-through: no inverse calculated yet, so solve and store it before returning the value
  real_mat <- x$get()
  inverse <- solve(real_mat)
  x$setinverse(inverse)
  inverse
}
