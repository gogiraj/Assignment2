## The makeCacheMatrix function creates a special kind of matrix object. These
## special matrices allow for calculation and caching of their inverse when
## passed as arguments to the cacheSolve function.

## The makeCacheMatrix function takes a matrix 'x' as its argument. The function
## stores the value of the matrix and returns a list containing 4 functions.
## These functions are used to access and change both the value of the stored
## matrix and the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## Set and store a new value for the 'x' matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ## Access the stored value for 'x'
  
  ## Manually set and store a value for the inverse of 'x':
  setinverse <- function(inverse) inv <<- inverse
  
  ## Access the stored value for the inverse of 'x':
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function takes a makeCacheMatrix object 'x' as its argument.
## The function returns the inverse of 'x'. If the inverse has been calculated
## before, the function will retrieve its value from memory. If not, it will
## calculate the value of the inverse and store it in memory for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## Try to retrieve 'inv' from memory:
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If not possible to retrieve, calculate and store the inverse of 'x':
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}