## Below method makeCacheMatrix() creats a R object with setter/ getter methods
## It also adds 2 methods to get and set the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseValue) inv <<- inverseValue
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() method uses the passed makeCachedMatrix object and returns 
## the inverse using the solve function before using the solve function it 
## check to see of the inverse is already computed and retruend or not using getInverse function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
