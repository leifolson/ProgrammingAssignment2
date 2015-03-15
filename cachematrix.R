## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix:
#   inputs: 
#       x => a matrix
#   outputs:
#       list(set,           sets matrix variable   
#            get,           gets the matrix
#            setInverse,    caches the inverse of the matrix
#            getInverse     returns the cached inverse
#           )
# This function creates a special type of matrix that can cache its
# matrix inverse rather than recomputing it each time.  The returned
# "matrix" is actually a list of functions that allow the user to 
# manipulate the cache matrix by setting/getting the matrix and its
# inverse.
makeCacheMatrix <- function(x = matrix()) {
    # variable to store inverse of the matrix
    inverse <- NULL
    
    # set essentially allows initialization of the special cache matrix
    set <- function(mat){
        x <<- mat
        inverse <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set and get functions to set or obtain the inverse of the matrix
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    # returns a list data structure containing all the functions
    # required to manipulate the special cache matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve:
#   inputs: 
#       x => a cache matrix obtained from makeCacheMatrix()
#       ... => additional arguments passed to solve()
#   outputs:
#       inv => the inverse of the matrix
# This function takes a cache matrix as input.  It checks if the
# inverse has already been computed/cached and returns the result if so.
# Otherwise, the inverse is computed, cached, and returned
# Assumptions: x is an invertible matrix
#              x is the result of a call to makeCacheMatrix()
cacheSolve <- function(x, ...) {
    # get the inverse of x
    inv <- x$getInverse()
    
    # check if the inverse has been cached
    if(!is.null(inv)){
        # inverse has been cached so get it and return
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # inverse has not been cached so compute, store, and return inverse
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}
