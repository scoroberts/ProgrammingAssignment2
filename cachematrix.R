## Taking the inverse of a matrix can be a very expensive operation
## for very large matricies. These methods allow one to cache
## the expensive operation so that if called again, the value
## will be retuned much faster.  This technique is also known
## as memoization in computer science.
##
## Example:
##   > testm <- makeCacheMatrix(matrix(c(2:5),2,2))
##   > cacheSolve(testm)    #expensive operation
##   > cacheSolve(testm)    #cache hit on second run


## This function will create a special version of matrix
## which will encapsulate the matrix value as well as the
## inverse of the matrix. This contains getters and setters
## for these values.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This will check to see if there is a cached inverse value.
## If not found, the inverse will be calculated, caached and
## returned.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("cache hit")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
