## This file contains a wrapper for matrices which
## cache the latest computed inverse.

## Wraps a matrix and provides four functions:
##    set(mat)        sets the contained matrix; resets cache
##    get()           returns the contained matrix
##    getInverse()    returns the cached inverse or NULL
##    setInverse(inv) sets the cached inverse to inv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new_x){
        x <<- new_x
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(new_inverse) inv <<- new_inverse
    getInverse <- function() inv

    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Returns the inverse of the given CacheMatrix.
## Uses the cached value if available; if the cache is
## NULL, computes the inverse and update cache.

cacheSolve <- function(x, ...) {
    cached_inverse <- x$getInverse()

    if(!is.null(cached_inverse)){
        return(cached_inverse)
    }  

    mat <- x$get()
    inverse <- solve(mat, ...)
    x$setInverse(inverse)

    inverse
}

