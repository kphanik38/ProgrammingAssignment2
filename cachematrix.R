## This source file contains two R functions which cache potentially time-consuming computations.
## They compute the Inverse of a matrix which is provided as an input.
## By caching the matrix, sometimes which is a huge one, typically enables fast calculations and operations.

## It is assumed that the matrix supplied is always invertible and code for explicit checks is not included.

## "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse to the memory
## It uses function defined with-in and uses lexical scoping to pin the matrix to cache

makeCacheMatrix <- function(inMatrix = matrix()) {

	outMatrix <- NULL		## set the output matrix to null to clean-up earlier assignments

	set <- function(y) {	## set the lexical scoping of the variables and cache them
			inMatrix <<- y
			outMatrix <<- NULL
	}

	get <- function() inMatrix	## function to fetch the input matrix
	setsolve <- function(solve) outMatrix <<- solve	## use "solve" to inverse and set the matrix in cache
	getsolve <- function() outMatrix	## fetch the inverse matrix from the cache

	list(set = set, get = get,	## list all the functions defined with-in the function "makeCacheMatrix"
			setsolve = setsolve,
			getsolve = getsolve)
}

## "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the original matrix has not changed),...
## ... then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

	outMatrix <- x$getsolve()	## fetch the output (inverse) matrix from cache memory, by "getsolve"

	if(!is.null(outMatrix)) {	## return the inverse matrix, from cache, if it exists
		message("Getting/Using the Matrix Inverse from the cache/memory")
		return(outMatrix)
	}

	tempMatrix <- x$get()		## get the original matrix from cache 
	outMatrix <- solve(tempMatrix, ...)	## calculate the inverse of the matrix to ...
	x$setsolve(outMatrix)		## (a) pin it to cache/memory by calling "setsolve" and

	outMatrix		## (b) return it, for the first execution
}

