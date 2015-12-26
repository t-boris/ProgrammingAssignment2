## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## This pair of functions creates a matrix and caches the its inverse.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setInv <- function (i)
        inv <<- i
    getInv <- function ()
        inv
    list(
        set = set, get = get,
        setInv = setInv,
        getInv = getInv
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
