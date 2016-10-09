## These functions can cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    # The makeCacheMatrix function can store a matrix and cache its inverse. 
    # It takes a matrix as argument, stores it and returns a list with four functions as 
    # elements: set, get, setInverse and getInverse (see below) 

    inv <- NULL # set the value of the inverse to NULL
    
    set <- function(y) { 
        # This function resets the matrix with y and clears any previously calculated inv
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x # This function returns the stored matrix. Takes 0 arguments
    
    setInverse <- function(inverse) {
        # This function caches the inverse in the variable inv
        inv <<- inverse 
    }

    getInverse <- function() inv # It returns the inverse (if cached) or NULL
    
    # Return a list with the four functions as elements
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# The cacheSolve function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cacheSolve should retrieve the inverse 
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
