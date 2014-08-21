## cachematrix.R implements two functions makeCacheMatrix() and cacheSolve()
## to save time on inverting large matrices
## makeCacheMatrix() handles storage and retrieval for large matrices and their 
## inverts
## cacheSolve() returns the matrix inversion 


## macheCacheMatrix(x = matrix()) constructs a list of setter/getter functions for
##      an input argument x (the large matrix)
##      the inverted matrix
## ##### VARIABLES USED #########################################################
## x is the input matrix. 
## iM is a variable for storing the inverted matrix
## y is the variable storing x in the makeCacheMatrix() environment
## ##### FUNCTION IMPLEMENTATION ################################################
## function set stores x in the environment of function makeCacheMatrix
## function set stores x in the environment of function makeCacheMatrix
## function setinverted writes the inverted matrix to iM
## function getinverted returns the value of iM
## ##### Example usage ##########################################################
## (assume there exists a matrix called data)
## test <- makeCacheMatrix(data)
## test$get() returns the values of data
## test$getinverted() returns solve(data) or NULL


makeCacheMatrix <- function(x = matrix()) {
        iM <- NULL
        set <- function(y) {
                x <<- y
                iM <<- NULL
        }
        get <- function() x
        setinverted <- function(inverted) iM <<- inverted
        getinverted <- function() iM
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}


## cacheSolve(x, ...) returns the matrix inversion of a matrix argument x
## x must be a matrix in the format created by makeCacheMatrix
## Before computation of the matrix inversion cacheSolve() checks if 
## the inverted matrix is already stored and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iM <- x$getinverted()
        if(!is.null(iM)) {
                message("getting cached data")
                return(iM)
        }
        data <- x$get()
        iM <- solve(data, ...)
        x$setinverted(iM)
        iM
}
