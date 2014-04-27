## There are two functions in this script.  These will compute and
## cache the inverse of a matrix.


## This function creates a special matrix object to cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      setmatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      getmatrix <- function() x
      setinverse <- function(inv) m <<- inv
      getinverse <- function() m
      list(setmatrix = setmatrix, getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by 
## the first function above.  If the inverse has already been 
## computed, then this function will retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$getmatrix()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
