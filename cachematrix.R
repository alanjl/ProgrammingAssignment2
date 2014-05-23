## This script implements a cached-matrix inverse functionality.
## Two functions are used, makeCacheMatrix and cacheSolve.
##
## The first, makeCacheMatrix must be called first. This initialises the 
## object that implements the cache.
##
## The second function, cacheSolve returns the inverse of the matrix.
## This function only calculates the inverse if it has not yet been calculated.
## See the function comments below.

## makeCacheMatrix:
##
## This function initialises a class instance that implements
## a cached matrix inverse function
##
## The input matrix is stored in x
## The cached inverse is stored in inv
## get and set functions are used to store and retrieve the matrix 
## getinverse and setinverse get and set the inverse of the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##
## cacheSolve:
##
## Returns the inverse of the input matrix.
## Assumes: the input matrix is invertible.
##
## If the inverse has already been calculated that cached value is returned
## using the return statement,
## otherwise the inverse is calculated using solve and stored in inv and
## then returned as the last statement of the function.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
