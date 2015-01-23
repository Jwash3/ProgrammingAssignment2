## These two functions accept a matrix and stores it in the cache and holds 
## it for when the cacheSolve function is called. The functions are also used 
## to store the inverse of the matrix so R doesn't calculate it every time 
## the inverse is needed.

## The makeCacheMatrix function creates a list of functions that are used 
## in the cacheSolve function. It stores, or preserves, the state of the matrix 
## in the cache for future use.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    m <<- inverse
  }
  getinverse <- function() {
    m
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix. 
## The function checks to see if the inverse matrix is already 
##  calculated and if it is calculated, the function returns the phrase 
## "getting cached data" and returns the inverse matrix. If the inverse 
## matrix is not calculated already, then the function calculates the inverse 
## of the matrix using the solve function.

cacheSolve <- function(x, ...) {
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

