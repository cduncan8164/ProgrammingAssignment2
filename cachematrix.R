## The first will find if the inverse of the matrix already exists
## The second will solve the inverse of the matrix if it does not already exist

## Searches to see if the inverse of the matrix already exists

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(inv = inv, get = get, setinv = setinv, getinv = getinv)
}


## Solves the inverse of the matrix

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinv(inv)
	inv
}
