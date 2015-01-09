## The following functions create and cache the inverse of matrix given in parameter.

### The first function allow to specify and get a matrix and inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_) inverse <<- inverse_
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


### In the second function, if the inverse of matrix is cached, the cached value is returned. 
### If cached value of matrix inverse does not exist, it will be calculate by solve R function
### and save in cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}


