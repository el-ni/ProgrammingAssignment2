# Together, these functions create a matrix-like object that can cache its 
# inverse and compute the inverse of the matrix


# The makeCacheMatrix function creates a matrix-like object and functions
# which are used by the cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
		
	}
	
	get <- function() x 
	setinverse <- function(inverse) i <<- inverse 
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The cacheSolve function computes the inverse matrix.
# If the matrix is not cached (i.e. its value is null), the function uses the
# get function created above to get its value and then computes its inverse
# and caches it. 

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	
	if (!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
