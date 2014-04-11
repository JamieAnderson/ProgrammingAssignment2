## The functions makeCacheMatrix and cacheSolve, in conjunction with one another, solve
## and cache the inverse of a square matrix.


## makeCacheMatrix takes a matrix, and compiles a special list of functions needed to run 
## cacheSolve.

makeCacheMatrix <- function( x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) INV <<- inverse
  getInverse <- function() INV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks to see if the inverse of the matrix has already been cached.  If it has,
## cacheSolve returns that cached value.  If the inverse has not already been cached, cacheSolve,
## computes the inverse, caches it, and returns it.

cacheSolve <- function(x) {
  
  INV <- x$getInverse()
  if(!is.null(INV)) {
    message("retrieving cached data")
    return(INV)
  }
  matr <- x$get()
  INV <- solve(matr)
  x$setInverse(INV)
  INV
}
