## makeCacheMatrix and cacheSolve are made following the examples of 
## makeVector and cachemean.
## The basic differences are matrix in makeCacheMatrix instead of numeric in makeVector and 
## solve in cacheSolve instead of mean in cachemean.

## function to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	get <- function() { x }
	setInverse <- function(InverseMx) { m <<- InverseMx}
	getInverse <- function() { m }
	list(get = get, 
		 setInverse = setInverse, 
		 getInverse = getInverse
		 )	
	## to return a list of 3 functions
}

## function to compute the inverse of the special matrix returned by makeCacheMatrix
## if the inverse has already been calculated and the matrix has not been changed,
## cacheSolve returns the inverse from the cache. Otherwise it computes by solve.


cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)  ## inverse matrix by solve
	x$setInverse(m)
	m		
	## m to return a matrix that is the inverse of 'x'
}
