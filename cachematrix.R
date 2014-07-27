## The below makeCacheMatrix and cacheSolve functions are used together too
## to take the inverse of a matrix and cache the results

## makeCacheMatrix - creates a list with the following capabilities
## set or get the value of the matrix
## set or get the inversion of the matrix

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


## cacheSolve - retrieve a stored answere for the matrix inverse when stored
## in cache or solve for the inverse and store it in cache

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

