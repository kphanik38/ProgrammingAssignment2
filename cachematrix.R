## This source file contains two R functions which cache potentially time-consuming computations.
## They compute the Inverse of a matrix which is provided as an input.
## By caching the matrix, sometimes which is a huge one, typically enables fast calculations and operations.

## It is assumed that the matrix supplied is always invertible and code for explicit checks is not included.

## "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse to the memory
## it uses function defined with-in and uses lexical scoping to pin the matrix to cache

makeCacheMatrix <- function(inMatrix = matrix()) {

        outMatrix <- NULL					## set the output matrix to null to clean-up earlier assignments
		
		set <- function(y) {				## a function to set the lexical scoping of the variables and cache them
                inMatrix <<- y
                outMatrix <<- NULL
        }
        
		get <- function() inMatrix			## create a sourceable object for get function to fetch the input matrix
        setsolve <- function(solve) outMatrix <<- solve	## create a sourceable object to inverse the matrix using solve function and set it in cache
        getsolve <- function() outMatrix	## create a sourceable object to fetch the matrix and get the output (inverse) matrix from the cache
        list(set = set, get = get,			## list all the functions defined with-in the function makeCacheMatrix
             setsolve = setsolve,
             getsolve = getsolve)
}

## "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

        outMatrix <- x$getsolve()			## fetch the output (inverse) matrix from cache memory using the getsolve function
		
        if(!is.null(outMatrix)) {			## return the inverse matrix, fetched from the cache, if it exists, for subsequent executions
                message("getting cached matrix inverse")
                return(outMatrix)
        }

        tempMatrix <- x$get()				## get the original matrix which is already present in the cache 
		outMatrix <- solve(tempMatrix, ...)	## get the inverse of the matrix for ...
        x$setsolve(outMatrix)				## (a) pin it to cache/memory by calling "setsolve" function and

        outMatrix							## (b) return it, for the first execution
}

