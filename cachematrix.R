## This code consists of functions that create a square invertible matrix 
## and ensures that the inverse of the matrix is available in the cache environment.

## makeCacheMatrix consists of a list of functions used by 
## cacheSolve to get or set the inverse of a square matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get=get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function finds the inverse of a matrix. If the inverse exists in the cache,
## it is returned from there, else it is computed and stored in the cache.

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
  return (m)
  
}
