## These functions allow the user to create a cached version
## of an inversted matrix in order to save precious compurtational
## time

## This function creates a a matrix object which can also cache its inversion

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## This functions solves the inverse of the Cached matrix
## from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message('getting cached matrix data')
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
