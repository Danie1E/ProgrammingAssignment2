## create special "matrix" object that can cache its inverse;
## consists of four functions for getting and setting the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## computes inverse of "matrix" returned by makeCacheMatrix;
## if the inverse has been calculated already, it is retrieved from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    startMatrix <- x$get()
    inverse <- solve(startMatrix)
    x$setInverse(inverse)
    inverse
}
