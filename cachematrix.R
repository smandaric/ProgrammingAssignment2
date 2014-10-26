## Returns the inverse of a matrix and stores the result to cache.  
## If the inverse has been calculated before, it is loaded from cache instead of recalculating


## Returns list of functions for setting and getting the values of matrix x and the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set <- function(y){ #Set the matrix 
        x <<- y
        i <<- NULL
    }
    get <- function() x #Get matrix 
    setinv <- function(inv) i <<- inv #Set cached inverse
    getinv <- function() i # Get cached inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of matrix
## Loads a cached inverse if it exists. If not, the inverse is calculated and stored to cache.


cacheSolve <- function(x, ...) {

    i <- x$getinv()
    if (!is.null(i)){ # Check if cached inverse exists
        message("Getting cached data")
        return(i) # Cached inverse
    }
    data <- x$get()
    i <- solve(data, ...) # Calculate inverse
    x$setinv(i) # Save inverse to cache
    i
}
