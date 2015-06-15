## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### This function creates a special "matrix" object that can cache its inverse.
### It can read matrix as argument or cache it later.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # This function reads matrix and assigns it to y
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # This function reads the value of matrix
        get <- function() x
        setInv <- function(inverted) inv <<- inverted
        getInv <- function() inv
        # We are making a list. It will be possible to call it using list subsetting
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}
## Write a short comment describing this function
### This function computes the inverse of the special "matrix" returned by
### makeCacheMatrix above. If the inverse has already been calculated
### (and the matrix has not changed), then the cachesolve should retrieve
### the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        # Giving message if inverted matrix is cached
        if(!is.null(inv)){
                message("Getting cached matrix")
                return(inv)
        }
        message("Inverting matrix")
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
