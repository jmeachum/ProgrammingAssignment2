## The pair of functions below can be used together to calculate the inverse
## of a function and cache those values.

## The makeCacheMatrix creates a matrix object that contains a list of
## functions that will be used to set and get values of the matrix
## as well as allow it to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<--NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function will be used to solve the inverse of a matrix
## as well as cache those values so they can be looked up again without
## having to run the calculation again.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
