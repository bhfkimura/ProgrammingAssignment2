## makeCacheMatrix is a function that defines four functions: set, get, setinverse, and getinverse.
## The input x is a matrix whose inverse we are interested in.
## The values for x, inv, and the functions set, get, setinverse, and getinverse will be stored in an
## enviroment whose parent environment is the Global Environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is the function that calculates the inverse of our matrix. The input x should be the output of the
## function makeCacheMatrix. If the inverse was not calculate before (that is, x$getinverse() returns NULL), 
## then we use x$get to retrieve our matrix, calculate its inverse, and stores its value by using x$setinverse.
## Otherwise, if x$getinverse() does not return NULL, then the function cacheSolve returns the inverse which 
## was previously calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


