## The functions makeCacheMatrix and cacheSolve will cache the inverse of
## the matrix automatically and return the cached data when needed.

## This function creates an object which has functions to 
## cache the inverse of an matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(arg_inverse) inv <<- arg_inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function save and return the inverse of an matrix if it is not
## calculated before. otherwise, it just return the the cached inverse 
## value of a matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
