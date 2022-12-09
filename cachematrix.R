## Put comments here that give an overall description of what your
## functions do

# This function creates a inverse matrix object cache it 

makeCacheMatrix <- function(x = matrix()) {
  inv_matx <- NULL
  set <- function(y) {
    x <<- y
    inv_matx <<- NULL
  }
  get <- function() x
  setInverse <- function() inv_matx <<- solve(x)
  getInverse <- function() inv_matx
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse matrix or retrieve from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
