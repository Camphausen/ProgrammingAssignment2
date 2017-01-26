## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object, 
## which contains a list of functions to set and get a matrix 
## i.e. store in current environment as "c_x", 
## and set and get the inverted matrix stored as "inverse_x"
##

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    c_x <<- y
    inverse_x <<- NULL
  }
  set(x)
  get <- function() c_x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix
## if a previous call of cacheSolve
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinverse(inv_x)
  inv_x
}


## Test workflow
## Create matrix: x<-matrix(c(2,3,5,6,7,8,10,12,13),nrow=3,ncol=3)
## make cache matrix: cm<-makeCacheMatrix(x)
## cm is a list with 4 values, the functions $set, $get, $setinverse, $getinverse 
## makeCacheMatrix calls cm$set(x) to store x in the global variable c_x
## and make inverse_x<<-NULL 
## cacheSolve(cm) either retrieves the cached value of inverse_x via $getinverse()
## or calculates inv_X and stores it in inverse_x via $setinverse(inv_x)
## the second call of cacheSolve(cm) displays a message about the cached matrix  


