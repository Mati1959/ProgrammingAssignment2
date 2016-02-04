## Composite matrix functions
## Calculate the inverse and store the inverse in the composite structure

## Create a composite matrix that stores its own inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
 
  get <- as.matrix(x)
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
  



## Calculate the inverse matrix if not in cache
## Return inverse from cache if in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get
  m <- solve(data, ...)
  x$setinv(m)
  m
}
