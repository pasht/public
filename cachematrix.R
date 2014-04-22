## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix caches a copy of the matrix and its inverse 
# passed in as the first argument

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
   set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
# cacheSolve retures the cached inverse of a matrix
# that has been previously been cached using makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
