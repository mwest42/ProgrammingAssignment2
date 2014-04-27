## These functions will find the inverse of a matrix and cache it. If the 
## inverse of the same matrix has to be calculated more than once (for example
## if it's called in a loop), a cached value can be returned instead of
## re-calculating.

## This first function will create a set of functions that set the matrix,
## get the matrix, set the inverse of the matrix, and get the inverse of
## the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
}


## This second function, using the first function, will check to see if the value
## of the inverse matrix is already cached, and if so it will return the matrix.
## If not, the inverse matrix will be calculated, cached, and the value returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
