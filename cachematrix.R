## makeCacheMatrix
## cacheSolve

## makeCacheMatrix Caches the inverted matrix for further use

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve determines if the inverse is cached or not. If it's cached, it returns the cached data, else
## it computes the inverse, caches it and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("inverse cached")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
