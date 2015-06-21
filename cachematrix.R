## The following functions help to create a cached Matrix and compute 
## the inverse when needed. 

## makeCacheMatrix is a function that takes a matrix and returns 
## a list of functions that can be used for caching the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(v) i <<- v
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve does the following:
## If the cached inverse is not null, it returns the cached value
## otherwise, it computes the inverse, update the cache and return the value.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
