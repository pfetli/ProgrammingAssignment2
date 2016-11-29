## === Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This is a nice example for lexical scoping.
## The following pair of functions caches the inverse of a matrix.

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
## It contains the following functions (for more information see alos
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md):
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  # The object inv is initialized within the function and will be used later on in the code
  inv <- NULL
  # set() function
  # Assigns the input argument to the x object in the parent environment
  # Assigns the value of NULL to the inv object in the parent environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get() function
  get <- function() x
  # set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # get the inverse of the matrix
  getinverse <- function() inv
  # Create a new object by returning a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## This function calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  # access the function
  inv <- x$getinverse()
  # in case the cach is not empty
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  # in case the cache is not empty
  # get the data
  data <- x$get()
  # calculate the inverse
  inv <- solve(data)
  # cache the result
  x$setinverse(inv)
  # return the inverse
  inv
}


