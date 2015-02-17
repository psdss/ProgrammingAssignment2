## These functions are for the Coursera Data Science Specialization R Programming
## Course Programming Assignment 2.  These functions create a special matrix
## and calculate the inverse of the original matrix. The inverse matrix is cached
## so if the function is called a second time then the result will be taken from
## the cache.


## Function: makeCacheMatrix(x)
## Parameters: x is the matrix for which the inverse is to be calculated.
## Return: returns a list of the functions available for the matrix
## Description:
## This function creates the inverse of the matrix using the Super Assignment
## operator to store the result at a higher level in the environment hierarchy
## thereby allowing it to remain between invocations.  A number of functions 
## are defined, including set, get, setIM, and getIM.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                   ## m is the inverse matrix.
    set <- function(y) {        ## set assigns the content of the original matrix   
        x <<- y
        m <<- NULL
    }
    get <- function() x         ## get returns the content of the matrix
    setIM <- function(solve) m <<- solve  ## setIM calculates the inverse
    getIM <- function() m       ## getIM returns the inverse matrix
    list(set = set, get = get,
         setIM = setIM,
         getIM = getIM)         ## return value is a list with the functions
}


## FUNCTION: cacheSolve(x, ...)
## Parameters: x is the original matrix for which the inverse is to be calculated.
## Return: Returns a matrix with the inverse of the original matrix
## Description: 
## This function checks to see if the inverse matrix has been calculated and if it
## has that is returned, otherwise the setIM function is invoked to calculate it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getIM()              ## getIM is the Get Inverse Matrix function
    if(!is.null(m)) {           ## if the inverse already exists, use it
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)       ## if it doesn't exist, calculate it using solve
    x$setIM(m)
    m
}