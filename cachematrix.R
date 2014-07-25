## Assignment 2 R Programming MOOC
## July 25 2014
## Hello from Portland, Oregon, USA!

## makeCacheMatrix creates a list that stores 
## a set of functions. It takes a matrix x as its argument.
## The first function, 'set' stores a matrix and allows  
## x to be replaced by another matrix y. The set 
## function also deletes existing values of the inverse. 
## The second function, 'get' retrieves the matrix x. 
## The third function, setinv, allows the inverse to be stored
## after it has been computed (generally by cacheSolve).
## The last function, getinv, retrieves the value of 
## the inverse if it has been stored (or NULL if it has
## not been stored).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of x. It (typically) takes a 
## list created by makeCacheMatrix as the argument. 
## First cacheSolve checks whether the computed inverse has already 
## been stored (cached) by makeCacheMatrix. If so, 
## it returns the cached value. If the inverse has
## not been stored, it computes the inverse of x
## and then stores it in the makeCacheMatrix list. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
