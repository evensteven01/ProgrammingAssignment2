## These functions work together to compute the inverse of matrix. The inverse
## is cached so rerunning will save time by getting the value from the cache
## instead of calculating it again.

## This function provides the interface for setting the cache of the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<-NULL
    }
    get <- function() x
    setinverse <- function(inver) inv <<- inver
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
  
}


## This function receives a cache matrix interface. It then either solves the
## inverse of the matrix and stores it in cache, or gets the pre-computed
## answer from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <-solve(data, ...)
    x$setinverse(inv)
    inv
}
