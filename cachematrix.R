## Below are two functions that will create an object to store
## a matrix and cache its inverse.

## This function will create a matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverseValue <- NULL
	set <- function(y) {
		x <<- y
		inverseValue <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inverseValue <<- inverse
	getinv <- function() inverseValue
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will compute the inverse of the matrix object return
## by makeCacheMatrix above. If the inverse is already calculated
## and the matrix has not changed, then the cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverseValue <- x$getinv()
	if(!is.null(inverseValue)) {
		message("getting cached data")
		return(inverseValue)
	}
	matrixValue <- x$get()
	inverseValue <- solve(matrixValue, ...)
	x$setinv(inverseValue)
	inverseValue
}