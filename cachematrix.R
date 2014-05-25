## write a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_m <<- inverse
  getinverse <- function() inv_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached inverse")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setinverse(inv_m)
  return(inv_m)
}