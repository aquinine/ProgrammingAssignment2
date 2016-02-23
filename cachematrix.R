## Put comments here that give an overall description of what your
## functions do

## There are two functions in this function.  The makeCacheMatrix
## function creates a special "matrix" that caches the inverse.  The 
## cacheSolve function uses the cached inverted matrix.

## makeCacheMatrix function takes a matrix as input and returns a 
## special "matrix" that caches the inverted matrix.  The function
## includes private setters and getters for making/returning the 
## "matrix" and the inverted "matrix."  The input matrix is assumed
## to be square and invertable.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes a "matrix" created by
## the makeCacheMatrix and returns the inverted matrix
## value.  If the "matrix" has a cached inversions, it
## returns that value, instead of re-calculating the 
## inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getinverse())) {
                message("getting cached data")
                return(m$getinverse())
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
