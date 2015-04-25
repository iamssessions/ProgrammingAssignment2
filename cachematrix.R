# This function creates a wrapper object for a matrix 
# that can cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function takes a makeCacheMatrix object as an argument
# and returns its inverse. If the makeCacheMatrix object has
# already cached the inverse of the underlying matrix, then 
# the cached value is returned, otherwise the inverse is computed
# and then cached and then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
