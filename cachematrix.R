## The following two functions create a matrix and create and cache its inverse
## matrix

## This function makeCatcheMatrix sets a matrix, gets this matrix,
## sets the inverse of a Matrix and gets this inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function(x) inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## This function cacheSolve first checks if the inverse of the matrix
## created above has been calculated already. If not it does so. In
## the end it returns the inverse matrix - either after calculation or
## from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
