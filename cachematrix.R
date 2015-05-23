## These functions try to find the inverse of the matrix and make a cache of it
## If the matrix is the same as before, it will use the cache

## This function will make a cache of the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function will give an inverse of the matrix 'x'
## If there already is the inverse of the matrix in the cache, it will show a message 'getting cached data'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
