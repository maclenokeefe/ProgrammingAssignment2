## Together these functions cache the inverse of a matrix
## The 1st creates a matrix object that can cache it's inverse
## The 2nd checks if the inverse has been calculated, 
## ...in which case it retrieves it, and if not, it computes it

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculated the inverse of the special matrix (makeCacheMatrix)
## If the inverse was already calculated, it retrieves it from the cache.

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