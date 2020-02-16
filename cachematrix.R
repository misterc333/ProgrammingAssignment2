## makeCacheMatrix creates an object that stores a matrix and its inverse,
## cacheSolve retrieves the inverse matrix from the cached value stored in 
## makeCacheMatrix() object's environment

## makeCacheMatrix description
## Takes a matrix object as input and creates an object that contains:
## 1. Two data objects: x and m;
## 2. Four functions which are returned to the parent environment via list():
## set() caches x (matrix from function argument) and m (as empty object) 
## in parent environment: previous cached m values (inverse matrix) are cleared 
## get() retrieves x matrix from parent environment
## setinverse() assigns inverse matrix to empty m object from parent environment
## getinverse() retrieves inverse matrix value m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() description
## The value of inverse matrix is retrieved from the parent environment
## (as stored in the makeCacheMatrix object) and a message is outputted 
## indicating this retrieval has occured 
## Otherwise, inverse matrix is calculated with matrix value x cached
## in parent environment (as stored in makeCacheMatrix object)
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


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
# returns inverse matrix of m1, second call returns inverse with message

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
# returns inverse of n2 object
