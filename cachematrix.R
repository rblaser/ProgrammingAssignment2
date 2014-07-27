## Overall description of what my functions do:
##   Matrix inversion is usually a costly computation.
##   There may be some benefit to caching the inverse of
##   a matrix rather than compute it repeatedly.  Below i
##   provide a means of cacheing the inverse so that it can
##   be recalled.

## Short comment describing this function:
##   This function creates a list of functions that when the 
##   functions are called, they can create the inverse or 
##   cache the inverse of a matrix. This function 
##   does not create the inverse, but can cache it when called.

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

## Short comment describing this function:
##   This function computes the inverse of the matrix returned
##   by makeCacheMatrix().  It only does this if the inverse
##   has not already been calculated.  If it has altready been calculated,
##   then it retrieves it from the cache.

cacheSolve <- function(x, ...) {
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

## To test it, uncomment the lines below:
x <- matrix(1:4, 2, 2)
a <- makeCacheMatrix(x)
cacheSolve(a)
