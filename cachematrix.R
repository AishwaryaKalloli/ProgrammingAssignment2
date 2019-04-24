# Functions to cache inverse of matrix and to fetch cached inverse matrix when available

# function to store and retrieve matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {

	# set default value of inverse to NULL
	inv <- NULL

	# function to set a matrix, default inverse is NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	# function to get the matrix
	get <- function() x

	# function to set inverse of the matrix
	setinverse <- function(inverse) inv <<- inverse

	# function to get inverse of matrix, 
	# it is NULL if not set using setinverse funtion
	getinverse <- function() inv

	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


# function that returns inverse of input matrix from cache if present, otherwise after calculation
cacheSolve <- function(x, ...) {
	# get inverse of x
	inv <- x$getinverse()

	# if it exists in cache return it
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	# else compute the inverse 
	data <- x$get()
	inv <- solve(data, ...)

	# store it in cache
	x$setinverse(inv)
	inv
}


