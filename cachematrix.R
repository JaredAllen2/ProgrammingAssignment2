## Put comments here that give an overall description of what your
## functions do

## establish a retrievable cache to store a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  ##initialise matrix inverse to NULL value
  matinv <- NULL
  ##declare function to cache matrix inverse
  set <- function(y) {
    x<<- y
    matinv <<- NULL
  }
  ##get matrix inverse value
  get <- function() x
  ##use solve function to calculate inverse of matrix
  setsolve <- function(solve) matinv <<- solve
  ##get the inverse
  getsolve <- function() matinv
  ##pass to makeCacheMatrix
  list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## If matrix inverse is available in cache, retrieve, else solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getsolve()
  ##retrieve if inverse already calculated
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setsolve(matinv)
  matinv
}
