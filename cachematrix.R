## These functions create a caching mechanism for the inverse of a matrix. When it is asked to inverse
# a matrix, it first checks in its cache whether the inverse has been calculated before, if it has, it returns
# the inverse from the cache otherwise it recalutes it and sets the value back to the cache

## This is the function representing the cache, setting and getting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This method checks whether the inverse of a matrix is already in the cache, if so, it returns that value, if not, recalculates
## and stores in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
