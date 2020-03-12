## cachematrix.R contains two functions: (1) makeCacheMatrix and (2) cacheSolve.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It takes as input a square invertible matrix and outputs a list containing
## functions that set and get the value of the matrix and its inverse.

## cacheSolve uses the solve function in R to calculate the inverse of a the
## square invertible matrix. This function uses the list from makeCacheMatrix.
## If an inverse was previously calculated, this function returns the value 
## from the cache and skips the computation.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special matrix object that can cache its inverse.
    ## Input:   A square invertible matrix
    ## Output:  A list containing the following functions:
    ##          1. set - sets the value of the matrix
    ##          2. get - gets the value of the matrix
    ##          3. setinverse - sets the inverse of the matrix
    ##          4. getinverse - get the inverse of the matrix
    
    ## initialize inv (an object that will contain the inverse of the matrix)
    inv <- NULL  
    
    ## set the value of the matrix 
    set <- function(y) {   
        x <<- y             
        inv <<- NULL
    }
    
    ## get the value of the matrix 
    get <- function() x 
    
    ## set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## get the value of the inverse
    getinverse <- function() inv
    
    ## retun the list containing the above functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    ## This function computes the inverse of a square invertible matrix.
    ## Input:   A list of functions from makeCacheMatrix
    ## Output:  The inverse of the original square invertible matrix used in
    ##          makeCacheMatrix
    ##          Note: If the inverse has already been calculated (and the matrix
    ##          has not changed), this function will return the inverse from the
    ##          cache.
    
    ## get the value of the inverse
    inv <- x$getinverse()
    
    ## check if the inverse has already been calculated
    if (!is.null(inv)){
        ## if the inverse has already been calculated, return value from cache
        message("getting cached data")
        return(inv)
    }
    
    ## otherwise, get the matrix and calculate the inverse 
    data <- x$get() 
    inv <- solve(data, ...)
    
    ## set the value of the inverse
    x$setinverse(inv)
    
    ## return/output the inverse of the matrix
    inv
}
