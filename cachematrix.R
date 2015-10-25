## Functions that enable the caching of a matrix inverse:
## One function creates a special matrix object that can cache 
## its matrix inverse,
## the other retrieves the cached inverse if computed already
## or computes the inverse if not.
## Code is based on the given Example in Programming Assignment 2.

## Function that creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Define function that sets the matrix:
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Define function that gets the matrix:
    get <- function() x
    ## Define function that sets the inverse of the matrix:
    setinverse <- function(inverse) m <<- inverse
    ## Define function that gets the inverse of the matrix:
    getinverse <- function() m
    ## Return the special matrix object, which is actually 
    ## a list containing the above four functions:
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that retrieves the cached matrix inverse 
## if computed already, or computes it if not
## (assuming the supplied matrix is invertible)

cacheSolve <- function(x, ...) {
    ## This function will return a matrix that is the inverse of 'x'
    ## First, get what is stored for the inverse:
    m <- x$getinverse()
    ## If the inverse has already been computed and cached 
    ## (and the matrix is unchanged),
    ## just get the cached inverse and return it:
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Otherwise, get the matrix itself, compute its inverse,
    ## cache it in the matrix object, and return it:
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
