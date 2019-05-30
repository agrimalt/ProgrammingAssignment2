## These functions allow storing a matrix with his inverse matrix, in a way that you only need to
## calculate the inverse once, and from that moment on no calculations will be made, the inverse matrix
## stored will be returned instead

## makeCacheMatrix contains the functions to assign a new value to the matrix itself and query this value,
## and the functions needed to query the matrix and its inversed too

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve queries the inverse of the matrix. If it had been calculated before, no 
## calculations is needed, so we can return the previous calculated inverse matrix;
## if inverse matrix hadn't been calculated yet, the calculation proceeds and stores it
## so that next time it can be returned without any more calculations

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
