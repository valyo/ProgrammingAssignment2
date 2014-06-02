## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## accepts a matrix and creates a list of functions for 
## setting and getting a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
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


## Write a short comment describing this function
## accepts the "list" returned by makeCacheMatrix
## and uses its functions to get the matrix, and calculate its inverse
## before inverse calculation it checks if inverse was not already calculated
## and cached
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
