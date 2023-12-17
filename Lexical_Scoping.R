# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix cache
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    # Reset the cached inverse when the matrix is set
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to get the cached inverse or calculate and cache it if not already present
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
  # Function to retrieve the cached inverse if available
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions to manipulate the matrix object
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Compute the inverse of the special matrix using caching
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inverse <- x$getInverse()
  
  # If the cached inverse is available, return it
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # If the cached inverse is not available, calculate it and cache it
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
