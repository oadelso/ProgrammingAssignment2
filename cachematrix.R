## makeCacheMatrix converts the input matrix 'x' into a list containing a funciton set the matrix, 
##get the the matrix, and either set the corresponding Inverse Matrix, or get the Inverse Matrix
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
  }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Gets the Matrix x, and if the inverse of x has been calculated, it returns the calculated matrix, otherwise it
##computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
                if(!is.null(m)) {
                 message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
