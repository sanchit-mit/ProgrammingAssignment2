## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix which is really a list containing a function
## that sets and gets the elements of the matrix and matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix and sets it in the cache 
##using the function "setinverse".


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...)
        x$setinverse()
        inv
}
