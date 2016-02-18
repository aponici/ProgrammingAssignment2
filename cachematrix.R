## This file contains two functions: makeCacheMatrix & cacheSolve
## Matrix inversion is usally a computational intensive operation,
## so the ability to cache the inverse of a matrix represents a 
## substantial improvement in performance. Combined, the two
## functions cache the inverse of matrix.

## This function creates a list containing a function that can (1) set
## the value of the matrix, (2) get the value of the matrix, (3) set 
## the inversed matrix, and (4) get the inversed matrix.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix given to the 
## makeCacheMatrix function. If the inverse has already been computed,
## it skips the computation and gives the cached value.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached inversed matrix")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv
}
