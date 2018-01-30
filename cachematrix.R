## makeCacheMatrix returns a special kind of Matrix object that provides the inverse of the matrix.
## cacheSolve creates the inverse of a matrix.  Attempts to get from cache, creates inverse only of not available in cache.

## Creates a special matrix object that also provides the inverse of a matrix.  Has methods to get and set data.

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


## Computes the inverse of a given matrix.  Initially looks for a cache of the inverse.  If not present it computes inverse
## and assigns computed inverse to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Look for inverse in the cache
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If cache not found then create inverse and store in cache.  Return inverse.
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
