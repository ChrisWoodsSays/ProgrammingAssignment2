## R Programming - Assignment 2

## Below are two functions:
## 1 - Creates a special "matrix" object that can
##     * Set it's value to that passed (set)
##     * Get it's value (get)
##     * Set it's cached inverse value to that passed (setInverse)
##     * Get it's cached inverse value (getInverse)
## 
## 2 - Computes the inverse of the special "matrix" returned by (1) above. 
##     If the inverse has already been calculated (and the matrix has not changed), 
##     then cacheSolve should retrieve the inverse from the cache.

## Creates a special matrix that can hold and return the matrix and a cache of it's inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Set the matrix to the passed value
  set <- function(y) {
    x <<- y
    ## When the matrix is set, empty it's cache
    i <<- NULL
  }
  ## Get the matrix to the passed value
  get <- function() x
  ## Set the inverse to the passed value
  setInverse <- function(inverse) i <<- inverse
  ## Get the inverse
  getInverse <- function() i
  ## Create a list witin the function containing the four subfunctions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of a matrix and uses makeCacheMatrix to store a cache of it
cacheSolve <- function(x) {
  i <- x$getInverse()
  ## If the cache is there, just return that
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
