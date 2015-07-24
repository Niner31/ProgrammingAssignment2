## This file contains two functions - "makeCacheMatrix" and "cacheSolve". 
## The purpose of these functions is to cache the inverse of a matrix rather
## than continuously repeat the computation and utilize the cached matrix 
## whenever it has been unchanged.

## The function "makeCacheMatrix" creates a special "Matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 1 - set the value of the matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## 2 - get the value of the matrix
    get <- function() x
    
    ## 3 - set the value of the inverse
    set_inverse <- function(matrixInverse) inv <<- matrixInverse

    ## 4 - get the value of the inverse
    get_inverse <- function() inv 
    list (set = set, get = get, 
          set_inverse = set_inverse,
          get_inverse = get_inverse)
}


## The function "cacheSolve" computes the inverse of the special "Matrix" object
## returned by the "makeCacheMatrix" function or (if the inverse has already 
## been calculated and has not changed), it will retrieve the cached version.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## 1 - check if the matrix is the same
    inv <- x$get_inverse()
    
    ## 2 - if matrix is same, return cached inverse
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    ## 3 - if matrix is not same, get the matrix to be inverted
    data <- x$get()
    ## calculate the inverse
    inv <- solve(data)
    ## set inverse in cache
    x$set_inverse(inv)
    ## return inverse
    inv
}
