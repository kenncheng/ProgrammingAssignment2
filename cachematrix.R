## This is an R function that is able to cache potentially time-consuming computations. 
## Matrix inversion is computation intensive and there is benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversematrix <<- inverse
        getinverse <- function() inversematrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above.  If the inverse has already been computed (and the matrix has not changed), then
## the cachesolve function should retrieve the inveerse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix
}
