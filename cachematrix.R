## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse matrix


## This function, makeCacheMatrix creates a "matrix" object with list containing functions to set and get the matrix, 
## as well to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invm <<- solve
  getinverse <- function() invm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returnes the inverse of "matrix" created with the above function. 
## It first checks to see if the inverse of matrix has already been calculated. 
## If so, it gets the result from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setinverse function.

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
