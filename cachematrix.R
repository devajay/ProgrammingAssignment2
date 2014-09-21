##Provides functions to implement caching of inverse of a matrix. 

## Creates a wrapper object which allows to get and set the wrapped matrix,
## set and get the inverse of a matrix. When a new wrapped matrix is set,
## it's inverse is reset. Set functions use super assignment to preserve
## the value in called environment.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function checks in the cache if the inverse is available for the 
## cacheMatrix. If available it returns the cached value, otherwise 
## computes the inverse, stores it in the cacheMatrix and then return 
## the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrithat is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv    
}

