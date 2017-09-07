## Compute the inverse of a matrix, caching the result.
## Retrieves the cached value on future calls.

## Create a list of functions containing getters and setters
## for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xi <<- inverse
    getInverse <- function() xi
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of the given matrix, retrieving a cached
## value if previously calculated.

cacheSolve <- function(x, ...) {
    xi <- x$getInverse()
    if (!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }
    matrix <- x$get()
    xi <- solve(matrix, ...)
    x$setInverse(xi)
    xi
}
