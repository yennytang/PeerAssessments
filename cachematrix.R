## These functions are used to cache the inverse of a matrix. 

## This makeCacheMatrix is used to create a matrix oject which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x} 
        setInverse <- function(solve) {
                m <<- solve} 
        getInverse <- function() {m}
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This cacheSolve function is used to compute the inverse of the matrix returned by makeCacheMtrix function. If the inverse has been computed (with the same matrix), it will return the inverse from the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setInverse(m)
        m
}
