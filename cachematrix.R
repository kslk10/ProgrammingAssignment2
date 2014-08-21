## Inversing of a matrix is a costly process. 
## The functions written here will cache this process in case this process in run continuiously in loop and matrix does not change.
## There in helps performance.

## Creates a specialized matrix as a list of get(),set(),getinverse(),setinverse()

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(mat) {
              x <<- mat
              i <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of a matrix when it is called first time, next time onwards it returns the cached inverse matrix if the matrix as not changed.


cacheSolve <- function(x, ...) {

	i <- x$getinverse()
        if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	mat <- x$get()
	i <- solve(mat)
	x$setinverse(i)
 	i
}
