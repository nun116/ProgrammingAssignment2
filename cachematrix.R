# Larry Miller - Assignment #2
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# The CacheSolve function below returns the inverse of the matrix. It determines if
# the inverse is already calculated. If the inverse is already calculated, results are pulled and the
# computation is skipped. If the inverse is not already calcultated, the inverse is calculated, and it sets
# the value in the cache using the setinverse function.

# The function below presumes the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retreiving cached data, ...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
## TEST examples:
## > x = rbind(c(1, -1/6), c(-1/6, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##          [,1]       [,2]
## [1,]  1.0000000 -0.1666667
## [2,] -0.1666667  1.0000000

## No cache in the first run
## > cacheSolve(m)
##         [,1]      [,2]
## [1,] 1.0285714 0.1714286
## [2,] 0.1714286 1.0285714

## Retrieving from cache in the 2nd run
## cacheSolve(m)
## Retreiving cached data, ...
##         [,1]      [,2]
## [1,] 1.0285714 0.1714286
## [2,] 0.1714286 1.0285714
## > 