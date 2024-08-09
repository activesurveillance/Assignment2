### Assignment: Caching the inverse of a matrix

## makeCacheMatrix is a function that creates a special matrix object that can cache its inverse ----

makeCacheMatrix <- function(x = matrix()) {
        # Initialise cached inverse to NULL ----
        inv <- NULL
        
        # Function to set the matrix value ----
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Invalidate cache when matrix is updated
        }
        
        # Function to get the matrix value ----
        get <- function() {
                x
        }
        
        # Function to set the cached inverse ----
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        # Function to get the cached inverse ----
        getInverse <- function() {
                inv
        }
        
        # Return a list of the above functions ----
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' ----
        
        inv <- x$getInverse()
        
        # If the cached inverse exists, return it ----
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, compute the inverse ----
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the computed inverse ----
        x$setInverse(inv)
        
        # Return the computed inverse ----
        inv
}
