## The following functions provide the ability to store the inverse of a matrix
## in memory so that the inverse doesn't have to be calculated repeatedly.
## The matrix and it's inverse are stored together in a list along with
##    the method to calculate the inverse.

## The following code creates a special matrix that is accessibile via a list.
## The function returns a list that stores the matrix and the inverse of the matrix.
## The underlying list provides the ability to read and change the value of the matrix.
##    as well as the ability to read and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function retrieves the inverse of the underlying matrix.
## If the inverse has not been cached then the inverse is calculated and cached
## in the supporting list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
