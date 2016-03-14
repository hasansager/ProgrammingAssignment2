## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix


## This function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the mean
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        set <- function(y) {
                x <<-  y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(val) inverse <<- val 
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of the matrix in argument
## the matrix has to be created by using the function makeCacheMatrix.
## Ff the inverse was once calculated it will be cached and use for later call of this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
