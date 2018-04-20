# Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        invrs <- NULL 
        set <- function(y) { 
                x <<- y 
                invrs <<- NULL 
                }
        get <- function() x 
        setinverse <- function(inverse) invrs <<- inverse 
        getinverse <- function() invrs 
        list(set = set, 
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        # If the inverse has already been calculated
        if (!is.null(invrs)) {
                # get it from the cache and skips the computation.
                message("getting cached data")
                return(invrs)
        }
        # Otherwise, calculates the inverse 
        data <- x$get()
        invrs <- solve(data, ...)
        
        # Sets the value of the inverse in the cache via the setinv function.
        x$setinverse(invrs)
        invrs
}

