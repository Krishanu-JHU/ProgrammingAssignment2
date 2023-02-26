makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setmat <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  
  getmat <- function() x
  
  cache <- function(solve) {
    if (!is.null(inv)) {
      message("Getting cached data.")
      return(inv)
    }
    inv <- solve(x)
    inv
  }
  
  list(setmat = setmat, getmat = getmat,
       cache = cache)
}

cacheSolve <- function(x, ...) {
  inv <- x$cache(solve)
  inv
}
