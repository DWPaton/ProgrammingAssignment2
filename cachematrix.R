
## MakeCacheMatrix creates a list of functions to 
## get and set the values of the matrix and its inverse

makeCacheMatrix <- function(x = numeric()) {
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



#cacheSolve returns an inverted matrix. It first checks to see if an
#inverted matrix has already been calculated and if it has it returns the
#cached matrix. If the inverted matrix has not been calculated, 
#it performs the calculation, stores the value in the cached 
#and variable and returns the inverted matrix

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

