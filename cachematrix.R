## This makeCacheMatrix function shown below  generates a particular "matrix" 
##item capable of caching its inversion.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The inverse of the special "matrix" returned by makeCacheMatrix is computed with this cacheSolve  function. The cachesolve should get the inverse from the 
## cache if the inverse has previously been calculated and the matrix has not changed.

cacheSolve <- function(x, ...) {
     
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
