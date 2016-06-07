## Put comments here that give an overall description of what your
## functions do
## create a vector list of functions to set a matrix, get matrix, set invers
## get inverse, then return a matrix of inverse of x, whether from cache or new calculation.


## This function creates a vector list of functions to set the matrix, get the matrix
## set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## this function calculates the inverse of a matrix, but checks first if the 
## inverse has already been calculated, if it has been it reports the calculation
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
