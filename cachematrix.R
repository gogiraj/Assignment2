## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix
makeCacheMatrix <- function(x = numeric()) {
    
    # initially nothing is cached so set it to NULL
    cache <- NULL
    
    # storing matrix
      setMatrix <- function(newValue) {
          x <<- newValue
      
          # flusing cache as matrix is assigned a new value
          cache <<- NULL
      }
    
    # returns stored matrix
      getMatrix <- function() {
          x
      }
    
    # cache given argument 
      cacheInverse <- function(solve) {
        cache <<- solve
      }
    
    # get the cached value
      getInverse <- function() {
        cache
      }
    
    # return a list: Each named element of the list is a function
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}




# cacheSolve:
cacheSolve <- function(x, ...) {
    # get the cached value
      inverse <- x$getInverse()
        # if a cached value exists return it
          if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
          }
      # otherwise get the matrix, caclulate the inverse and store it in cache
  
      data <- x$getMatrix()
      inverse <- solve(data)
      x$cacheInverse(inverse)
  
    ## Return a matrix that is the inverse of 'x'
    inverse
}