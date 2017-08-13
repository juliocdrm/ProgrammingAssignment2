## Put comments here that give an overall description of what your
## functions do

## Function that receive a Matrix and creates a cache, in order to better user the computational resources

makeCacheMatrix <- function(mx = matrix()) {
        inv <- NULL
        set <- function(mx2) {
                mx <<- mx2
                inv <<- NULL
        }
        get <- function() mx
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## The follow function returns the inverse of matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
