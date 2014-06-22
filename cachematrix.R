## This source file contains two R functions which cache potentially time-consuming computations.
## They compute the Inverse of a matrix which is provided as an input.
## By caching the matrix, sometimes which is a huge one, typically enables fast calculations and operations.

## It is assumed that the matrix supplied is always invertible and code for explicit checks is not included.

## this function creates a special "matrix" object that can cache its inverse to the memory
## it uses function defined with-in and uses lexical scoping to pin the matrix to cache

makeCacheMatrix <- function(inMatrix = matrix()) {

## set the output matrix to null to clean-up earlier assignments
        outMatrix <- NULL
		
## a function to set the lexical scoping of the variables for the function with-in
## and cache the variables to memory
        set <- function(y) {
                inMatrix <<- y
                outMatrix <<- NULL
        }
		
## create a sourceable object for get function to fetch the input matrix

        get <- function() inMatrix
		
## create a sourceable object to inverse the matrix using solve function and set it in cache

        setsolve <- function(solve) outMatrix <<- solve

## create a sourceable object to fetch the matrix and get the output (inverse) matrix from the cache

        getsolve <- function() outMatrix

## list all the functions defined with-in the function makeCacheMatrix

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

## fetch the output (inverse) matrix from cache memory using the getsolve function

        outMatrix <- x$getsolve()

## return the inverse matrix, fetched from the cache, if it exists and not NULL
## if this block is executed then the control skips the next commands as it encounters "return" statement

        if(!is.null(outMatrix)) {
                message("getting cached matrix inverse")
                return(outMatrix)
        }

## get the original matrix which is already present in the cache 

        tempMatrix <- x$get()

## get the inverse of the original matrix and assign it to the outMatix to (a) pin it in teh cache/memory and (b) return it

        outMatrix <- solve(tempMatrix, ...)

## use the setsolve function to store the inverse matrix in the cache

        x$setsolve(outMatrix)

## return the inverse of the original matrix...
## return for the first time and the control ends in the IF block for second execution and thereafter

        outMatrix
}

