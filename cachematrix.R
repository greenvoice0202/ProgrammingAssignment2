## Put comments here that give an overall description of what your
## functions do
## To inverse a matrix is a time-consuming computation.
## It can save time to do such inverse again for the same matrix. 
## That is, to cache the computed inverse matrix for later use.
## These two functions makeCacheMatrix and cacheSolve are a pair of functions to accomplish the jobs:
##	store the maxtrix and cache the inverse matrix

## Write a short comment describing this function

## This function makeCacheMatrix creates a special "matrix" object that can:
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse matrix
##	get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	invM <- NULL
	set <- function(y) {
        	x <<- y
        	invM <<- NULL
        }
        get <- function() x
        setInverseM <- function(inverse) invM <<- inverse
        getInverseM <- function() invM
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}


## Write a short comment describing this function
## This function calculates the inverse matrix of the special "matrix" created with the above function.
## It first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and 
## sets the value of the inverse matrix in the cache via the setInverseM function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invM <- x$getInverseM()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        theM <- x$get()
        invM <- solve(theM, ...)
        x$setInverseM(invM)
        invM
}
