## A pair of complementary functions to compute and cache the 
## inverse of a matrix.

## makeCacheMatrix decorates a "matrix" object with a
## list of functions which can
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
##
## Usage: makeCacheMatrix(x=matrix())
## Arguments:
##    x   optional square invertible matrix
##
## Examples:
## m <- makeCacheMatrix()
## 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        if (!identical(x, y)) {
            x <<- y
            i <<- NULL
        }
    }
    
    get <- function() x
    
    setinverse <- function(inv) i <<- inv
    
    getinverse <- function() i
    
    list(set=set, get=get, 
         setinverse=setinverse,
         getinverse=getinverse)

}


## cacheSolve computes and returns the inverse of a "matrix" bound
## to the special object returned by makeCacheMatrix. 
## If the inverse has already been computed 
## (and the matrix has not changed), then this function will 
## retrieve the inverse cached with the matrix object.
##
## Usage: cacheSolve(x, ...)
##
## Arguments:
##     x   The decorated matrix object returned by makeCacheMatrix
##     ... Optional additional arguments for "solve"
## 
## Example:
##     mm <- makeCacheMatrix(matrix(1:4, 2, 2))
##     inverse <- cacheSolve(mm)

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Getting cached inverse...")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
