## Series of functions that create a matrix, determines the inverse of that matrix,
## and then returns the inverse.  If the inverse is not in cache, then results are stored
## for future reference.

## Creates a matrix and then sets the results in cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Returns the inverse of a matrix, x, if it exists in cache.  
## If the inverse does not exists, the function returns null.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}