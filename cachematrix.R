## storing a martix and a cached value of the inverse of the 
## matrix. Contains the following functions:
## - setMatrix      set the value of a matrix
## - getMatrix      get the value of a matrix
## - cacheInverse   set the cached value (inverse of the matrix)
## - getInverse     get the cached value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setMatrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        getMatrix <- function() {
                x
        }        
        cacheInverse <- function(solve) {
                cache <<- solve
        }        
        getInverse <- function() {
                cache
        } 
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             cacheInverse = cacheInverse, getInverse = getInverse)
        
}

## The following function calculates the inverse of a "special" matrix 
## created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        cache <- x$getInverse()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        data <- x$getMatrix()
        cache <- solve(data)
        x$cacheInverse(cache)
        cache
}





