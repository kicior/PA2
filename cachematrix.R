## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Setting y to NULL makes possible checking if there is any matrix to invert
        y <- NULL
        # This function reads matrix and assigns it to y
        makeCache <- function(x) y <<- x
        # This function reads the value of matrix
        getCache <- function() y
        # We are making a list. It will be possible to call it using list subsetting
        list(makeCache = makeCache, getCache = getCache)
}
## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Assigning cached (or not) matrix to flag y makes possible to do
        # different things depending of its presence or absence
        y <- x$getCache()
        # Giving message if no matrix present
        if(is.null(y)){
                message("No matrix to invert")
        }
        # Giving message if matrix is present but not square
        else if(!(dim(x$getCache())[1]==dim(x$getCache())[2])){
                message("Matrix must be square")        
        }
        # Inverting matrix and giving message
        else {
                message("Inverted matrix is:")
                mInverse <- solve(x$getCache())
                mInverse
        }
}
