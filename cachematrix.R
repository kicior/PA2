## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        y <- NULL
        makeCache <- function(x) y <<- x
        getCache <- function() y
        list(makeCache = makeCache, getCache = getCache)
}
## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y <- x$getCache()
        if(is.null(y)){
                message("No matrix to invert")
        }
        else if(!(dim(x$getCache())[1]==dim(x$getCache())[2])){
                message("Matrix must be square")        
        }
        else {
                message("Inverted matrix is:")
                mInverse <- solve(x$getCache())
                mInverse
        }
}
