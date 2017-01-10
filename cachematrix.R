## These functions contain caching functionality to store matrix inversions,
## so the inversion is calculated only once, and after that the previously calculated inverse is used.


## Creates and returns an object that stores the given matrix and inverse of the matrix.
## Functions for getting and setting the inverse matrix are also included in the returned object.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes a matrix object created by makeCacheMatrix, and returns its inverse.
## If the inverse was calculated previously, the precalculated version is used.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
