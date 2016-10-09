## These functions cache the inverse of a matrix

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) { 
        # This function resets the matrix with y and clears any previously calculated inv
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x # It returns the stored matrix. Takes 0 arguments
    
    setInverse <- function(inverse) {
        # It stores the calculated inverse in the variable inv
        inv <<- inverse 
    }

    getInverse <- function() inv # It returns the inverse (if cached) or NULL
    
    # Return the list with the four functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# The cacheSolve function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
    
    # Check if the inverse has alreasy been cached
    inv<- x$getInverse() # x$getInverse() will return NULL if inv is not cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Get the matrix
    data <- x$get()
    
    # Calculate the inverse of the matrix
    inv <- solve(data)
    
    # Store the inverse
    x$setInverse(inv)
    
    # Return inverse
    inv
    
    }
