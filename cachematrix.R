## Programmed by Emily Harris ProgrammingAssignment2
## The purpose of program is to write two functions: The first makeCacheMatrix
## creates an invertible matrix for later computations
## The second cacheSolve returns the inverse of the cacheMatrix

## makeCacheMatrix function creates a square matrix and saves a matrix object



makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set <- function(y) {

		x <<- y
		inv <<- NULL


	]

	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolves returns the inverse of the invertible square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv = x$getInverse()
	if(!is.null(inv)) {

		message("retrieving cached data")
		return(inv)

	}

	matrixA <-x$get()
	inv <- solve(matrixA,...)
	x$setInverse(inv)
	inv
}

}


