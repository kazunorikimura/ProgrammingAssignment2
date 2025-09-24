## Overall description:
## These functions cache the inverse of a matrix to avoid
## redundant computations, which can be costly for large matrices.


## Creates a special "matrix" object that can store a matrix and its inverse.
## Provides functions to:
## 1. set(): update the matrix
## 2. get(): retrieve the matrix
## 3. setinv(): cache the inverse
## 4. getinv(): get the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Computes the inverse of the special "matrix" object.
## If already cached, returns the cached inverse. Otherwise, computes,
## caches, and returns the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinv()　
  if(!is.null(i)) { 
    message("getting cached data")  　
    return(i)
  }
  data <- x$get() 
  i <- solve(data, ...)
  x$setinv(i)　
  i  
}



