## The below two funtions cache the inverse of a matrix rather 
## than compute it repeatedly, if the matrix has not changed. 



## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (& the matrix has not changed), then this 
## function only retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #calculate inverse since the cache is empty
  data <- x$get()  
  i <- solve(data, ...)
  x$setInverse(i)
  i 
}
