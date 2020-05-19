## Week 3 R programming course peer graded Assignment
## Jose Castillo Rabazo
## These functions aim to cache a matrix and make the inverse in order to
## save computing power and make programs more efficients.

## makeCacheMatrix creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Retrieves the inverse matrix cached by makeCacheMatrix().
## It requires and argument from makeCacheMatrix().

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
