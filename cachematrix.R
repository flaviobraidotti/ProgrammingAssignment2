## These are a pair of functions that cache the inverse of a matrix

## This fucntion creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## How to get the matrix
  get <- function() {
    m
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  
  getInverse <- function() {
    i
  }
  
  ## Return a list of the methods
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix" here above
## If the inverse already calculated (and the matrix has not changed)
## then "cachesolve" function below should get the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Just return the inverse if it is already set
    if( !is.null(m) ) {
      message("getting cached data")
      return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculating the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}
