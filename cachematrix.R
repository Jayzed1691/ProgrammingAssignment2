## These two functions take a square matrix as input and calculate and store the matrix
## inverse for use in later functions

## This first function creates a vector object with four functions that can get or set a matrix
## and get or set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Initialize the inverse matrix cache to Null
    set <- function(y) {
        x <<- y
        m <<- NULL
        ## Accept a new matrix assigned to x and re-initialize the inverse m to Null
    }
    get <- function() x
    ## Returns the matrix
    setinverse <- function(inv) m <<- inv
    ## Stores the inverse matrix into setinverse
    getinverse <- function() m
    ## Returns the inverse matrix
    list(set = set, get = get, setinverse=setinverse,
         getinverse = getinverse)
}


## This function computes, caches and returns the inverse of the special"matrix" returned by `
## makeCacheMatrix` above after checking to determine that the inverse has not already been
## calculated and stored

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' and cache the result
    m <- x$getinverse()
    ## First initialize m with whatever value currently resides in x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        ## If m has already been assigned the inverse of a matrix, leave this function and return 
        ## the cached value of m
    }
    data <- x$get()
    ## If m returned NULL, assign the input matrix contained in x$get() to "data"
    m <- solve(data, ...)
    ## Calculate the inverse of the matrix "data"
    x$setinverse(m)
    ## Assign the inverse matrix to x$setinverse() so subsequent calls to cacheSolve will return the
    ## cached data
    m
    ## Return the inverse matrix
}
