## caching a matrix inverse
##
##
## use this format inserting the matrix:
##    some_matrix$set(matrix(c(...), nrow=..., ncol=...))
## for printing the matrix
##    some_matrix$get()
## calculating the inverse   
##    cacheSolve(some_matrix)
## check if the caching works, repeat last line and get the message:
##    "getting cached data"


## functions for setting and getting the matrix and calculating its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}

## function for checking if the inverse was already calculated
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