## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored inverse value to NULL
    inv <- NULL

    # to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # since the matrix changed
    }
    # to get the value of the matrix
    get <- function() x
    # to set the inverse
    setinv <- function(inv_) inv <<- inv_
    # to get the inverse
    getinv <- function() inv

    # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed,
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    # check if the inverse is already cached
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # and compute the inverse
    inv <- solve(data, ...)
    # then cache the inverse
    x$setinv(inv)
    # and return it as well
    inv
}