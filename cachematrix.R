## This file contains two functions which help facilitate caching of a matrix
## inverse. This is useful, especially on large matrices, as matrix operations
## tend to be computationally expensive.

# This function takes a square matrix as an input. It is used as a function which
# creates a special "matrix" object which can cache its inverse.
# Input: (x) => Square matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function takes a square matrix as input, and returns the inverse of
# the matrix. As the inverse operation can be costly, this function returns
# a cached value after the first calculation of the inverse.
# Input: (x) => Square matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
