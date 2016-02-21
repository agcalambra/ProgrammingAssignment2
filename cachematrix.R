## cachematrix.R allows the creation of a matrix whose inverse
## can be cached in order to save inverse computation time for 
## larger matrices. The solution below was based on Roger Peng's 
## vector mean caching example.

## makeCacheMatrix creates a list that represents a matrix with
## a cacheable inverse.
##
## Usage:
## makeCacheMatrix(x)
##
## Arguments:
## x    the target matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve retrieves the inverse of a matrix if it exists
## in the matrix's cache. If not, it computes the cached inverse
## and stores it in the cache
##
## Usage:
## cacheSolve(x)
##
## Arguments:
## x    the target matrix whose inverse will be retrieved or solved
##      then cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
