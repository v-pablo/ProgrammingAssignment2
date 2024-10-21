## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # This will hold the cached inverse
    
    # Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Invalidate the cached inverse when the matrix changes
    }
    
    # Function to get the value of the matrix
    get <- function() x
    
    # Function to set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Function to get the value of the inverse
    getinverse <- function() inv
    
    # Return a list of the above functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

# makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
# It returns a list of four functions:
# 1. set: Updates the matrix stored in the special object and clears the cached inverse.
# 2. get: Retrieves the matrix stored in the special object.
# 3. setinverse: Caches the inverse of the matrix.
# 4. getinverse: Retrieves the cached inverse of the matrix (if available).
# This structure allows caching the inverse to avoid redundant calculations.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Check if the inverse is already cached
    
    # If the inverse is cached, return it and skip calculation
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse and cache it
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse using solve()
    x$setinverse(inv)  # Cache the inverse
    inv  # Return the computed inverse answer
}

