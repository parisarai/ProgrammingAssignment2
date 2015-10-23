## Two functions defined in this file, one returns special matrix object
## which caches a matrix, and 2nd returns inverse of the special matrix
#Assumption: All input matrices are invertible

## Returns a special object that caches inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	x <<- y
	m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## Inverse of the special object, retrieves inverse if an inverse exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
      if(!is.null(m)) {
                message("getting cached data")
                return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
