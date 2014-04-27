## This set of functions calculates the inverse of a matrix and
## saves the result so it does not need to be recalculated.

## Takes a matrix 'x'
## Returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize inverse as null
    inv <- NULL
    
    ## Replaces matrix 'x' with new matrix 'y' and delete 
    ## any saved inverse
    set <- function(y) {
          x <<- y
          inv <<- NULL
    }
    get <- function() x ## Returns the matrix 'x'
    setSolve <- function(solve) inv <<- solve ## Saves the inverse
    getSolve <- function() inv ## Returns the saved inverse
    
    ## Return a list of the four functions
    list(set = set, get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}


## Takes a matrix 'x' created with the function makeCacheMatrix
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Pull the matrix's cache so we can find out if it has
    ## already been calculated.
    inv <- x$getSolve()
    
    ## If the inverse of the matrix has already been calculated,
    ## then return the cached value
    if(!is.null(inv)) {
        message("Getting cached inverse.")
        return(inv)
    }
    
    data <- x$get()
    
    ## Inverse is not cached, so calculate the inverse
    inv <- solve(data, ...)
    
    ## Save the inverse with setSolve() so we do not have to
    ## calculate it again
    x$setSolve(inv)
    inv
}
