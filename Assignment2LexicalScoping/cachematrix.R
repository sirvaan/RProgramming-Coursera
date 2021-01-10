## Matrix inversion is usually a costly computation. The following functions
## provide possibility of caching the inverse of a matrix rather than compute 
## it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  set <- function(y){
    x <<- y # Set the value
    i <<- NULL # Clear the cache
  }
  
  # Define function to get the value of the matrix
  get <- function() x
  # Define function to set the inverse. This is only used by getinverse() when
  # there is no cached inverse
  setinverse <- function(inverse) i <<- inverse
  # Define function to get the inverse
  getinverse <- function() i
  
  # Return a list with the above four functions
  list( set = set , get = get,
        getinverse = getinverse,
        setinverse = setinverse)
}


## cacheSolve
# the following function calculates the inverse of the special "vector" created 
# with makeCacheMatrix. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the Solve() function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # This fetches the cached value for the inverse
  if(!is.null(i)) {   # If the cache was not empty, we can just return it
    message("getting cached data")
    return(i)
  }
  # The cache was empty. We need to calculate it, cache it, and then return it.
  data <- x$get()       # Get value of matrix
  i <- solve(data, ...) # Calculate inverse
  x$setinverse(i)       # Cache the result
  i                     # Return the inverse
}

