## Matrix as input
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ##Set the value of the matrix
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##Get the matrix
        get <- function() x
	##Assigns the inverse matrix (if available) to m
	setmatrix <- function(solve) m <<- solve
        ##Get the inverse matrix (if available)
	getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Gets the above matrix and calculate the inverse if not already calculated
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        ## if inverse is already calculated, display "getting cached data"
	## and return value of 'm'
	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        ## calculate inverse of the matrix
	m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
