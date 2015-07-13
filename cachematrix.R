## Put comments here that give an overall description of what your
## functions do

## stores the matrix in cache to avoid duplicated calculations
makeCacheMatrix <- function(x = matrix()) {
    ## set default inverse value as null
    inv <- NULL
    ## save the matrix
    set <- function(y) {
           x <<- y
           det <<- NULL
    }
    ## retrieve the matrix
    get <- function() x
    ## save the inverse matrix
    setinverse <- function(i) inv <<- i
    ## retrieve the inverse matrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function receives a cacheMatrix object, and
## returns its cached inverse if exists or calculate it
cacheSolve <- function(x, ...) {
    ## check if x has inverse calculated
    inv <- x$getinverse()
    ## if the inverse is cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## retrieve the matrix to calc the inverse
    data <- x$get()
    ## solve return the inverse of the matrix
    inv <- solve(data, ...)
    ## save the inverse into the cache
    x$setinverse(inv)
    ## return the inverse
    inv
}
