## Creates a special matrix that will holds functions that will act
## on the data it contains.

## Takes a matrix and stores the data and adds some functions
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	# Sets internal data to be the new given data	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}	
	# Returns the internal data
	get <- function() x

	# Caches the inverse
	setSolve <- function(solve) m <<- solve
	# Returns the cache
	getSolve <- function() m
	
	list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
	
}


## Finds the inverse of the matrix and caches the results.
## First it checks to see if a cache already exist. If so return cache.
cacheSolve <- function(x, ...) {
        m <- x$getSolve()
	# Checks to see if cached exists. 
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# No cached exist so compute inverse, cache result and return.
	data <- x$get()
	m <- solve(data, ...)
	x$setSolve(m)
	m
}
