## makeCacheMatrix contains a list containing a function to:
    ##  1. Set the value of the matrix
    ##  2. Get the value of the matrix
    ##  3. Set the value of the inverse matrix
    ##  4. Get the value of the inverse matrix
## The resulting list is stored in the variable m


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


## cacheSolve returns a matrix that is the inverse of 'x'
## If this has already been solved (if m is not NULL), then the "if" section is activated
##      and the function returns 'm' which is the stored matrix.
## If the inverse has not been calculated, then the 'solve' function performs the calculation
##      on the data passed to it as 'x' and stores the result in 'm' and returns the matrix.

cacheSolve <- function(x, ...) {
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
