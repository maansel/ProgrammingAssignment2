## This function contains four functions:
## set saves the passed matrix and initializes the cache
## get returns the value of the original matrix
## setinverse calculates the inverse of the matrix and stores it in a supervariable
## getinverse returns the cached inverse matrix
## used samplematrix <- matrix(c(1,2,3,5),nrow=2,ncol=2) to test

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns the inverse of the passed matrix, either from cache,
## if available, or by using the solve function, if not previously cached

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
