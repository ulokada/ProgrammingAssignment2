# `makeCacheMatrix` creates a special "matrix", which is
# is a list containing functions to
# 1.  set the matrix
# 2.  get the matrix
# 3.  set the value of the matrix inverse
# 4.  get the value of the matrix inverse
#
# The 'cacheSolve' function calculates the inverse of the special "matrix"
# but, it first checks to see if the inverse has already been calculated. 
# If so, it `get`s the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of
# the inverse in the cache via the `setinverse` function.


## Creates a special matrix with 'set' and 'get' functions for both
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(z) inv <<- z
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the matrix inverse and cache it or skips calculation
## and gets the value if it's already cached

cacheSolve <- function(x, ...) {
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
