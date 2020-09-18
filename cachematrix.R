## Put comments here that give an overall description of what your
## functions do

## This function creates special "matrix" which is a list containing the following functions:

# 1- set the value of the matrix
# 2- get the value of the matrix
# 3- set the value of the inverse
# 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix created with the function
## makeCacheMatrix. To avoid unnecessary computations it checks if the inverse
## has been already calculated and return the cached value. Otherwise the 
## inverse is calculated and stored.

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
