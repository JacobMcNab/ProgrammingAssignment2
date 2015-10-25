## Jacob McNab

## The function makes a special matrix that can cache it's inverse.
## Sets value of the matrix
## Gets value of the matrix
## Sets Inverse of the matrix (After cacheSolve)
## Gets Inverse of the Matrix (After cacheSolve, Before gives value "NULL")

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}



## calculates the inverse
## Caches the inverse and give message "caching data"

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("caching data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
