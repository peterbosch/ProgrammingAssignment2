## Creates a data structure that contains the provided
## matrix as well as a cache location for the matrix's
## inverse which is lazy-computed when requested, and 
## thereafter, if requested, simply returns the pre-
## computed inverse with no additional computational 
## load.
## 
## This structure is intended to be used with the 
## cachesolve function.
##
## Example usage:
## > myMatrix <- matrix(rnorm(1:25), 5, 5)
## > speedSolveMatrix <- makeCacheMatrix(myMatrix)
## > value1 <- cacheSolve(speedSolveMatrix)
## > value2 <- cacheSolve(speedSolveMatrix)
## >
makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  
  set <- function(y=matrix()){
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse = matrix()){
    cachedInverse <<- inverse
  }
  getInverse <- function() cachedInverse

  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## Given a data structure as created in makeCacheMatrix(x)
## above, computes and returns its inverse. If the inverse
## has already been calculated and returned, it returns that
# cached value.

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  matrix <- x$get()
  cachedInverse <- solve(matrix, ...)
  x$setInverse(cachedInverse)
  cachedInverse
}
