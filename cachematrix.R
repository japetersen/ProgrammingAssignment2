## These two functions together, overall, create a location
## to store and recall the output of an inverted matrix (or
## a cached matrix output).

## The makeCacheMatrix functions creates the location the 
## cached matrix output gets stored into. It essentially 
## creates a place holder for the output of the solve
## function.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set <-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The cacheSolve function returns the inverse of a matrix,
## but only if there is no output from an earlier call of
## the function. If there is a result that was cached earlier,
## it will return that result. Otherwise, cacheSolve will
## calculate and return the inverse of the matrix entered
## into the function.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
