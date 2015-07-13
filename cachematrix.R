## Put comments here that give an overall description of what your
## functions do

## stores the matrix in cache to avoid duplicated calculations
makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## check if x has inverse calculated
    inv <- x$getinverse()
    ## if the inverse is cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
