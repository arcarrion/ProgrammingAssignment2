## Functions to create a matrix that is able to cache its inverse.
## The goal of this type of matrix is to actually calculate its inverse 
## just when it is created or set. Otherwise, the cache of the inverse
## will be returned when asked.

## Creates a list of functions to get and set both the matrix passed
## as argument and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix represented by x.
## If the inverse is already calculated, the function
## returns the cached version.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
