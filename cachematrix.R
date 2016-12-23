## These functions create a matrix, and cache its value
## They also allow the inverse of the matrix to be calculated and cached
## If the inverse has already been calculated, then the cached value is returned
## If not, then the inverse is calculated and cached
## This is useful as calculating the inverse of a matrix is time-consuming

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Create an initial NULL inverse value
  inverse <- NULL
  # Function to cache the matrix value and reset the inverse (as the matrix has changed)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Function to return the matrix value
  get <- function() x
  # Function to cache the inverse of the matrix
  setInverse <- function(inverse) inverse <<- inverse
  # Function to return the inverse of the matrix (NULL when matrix has changed)
  getInverse <- function() inverse
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Get inverse value from cacheMatrix
  inverse <- x$getInverse()
  
  # If inverse contains a value...
  if (!is.null(inverse)) {
    # Return the result
    return (inverse)
    
  # Otherwise....
  } else {
    # Get the initial matrix from cacheMatrix
    data <- x$get()
    # Calculate the inverse
    inverse <- solve(data, ...)
    # Set the inverse in the cache
    x$setInverse(inverse)
    # Return the inverse (return() not technically required but makes code much clearer.)
    return (inverse)
  }
}
