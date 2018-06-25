## Put comments here that give an overall description of what your
## functions do
## Created By:  Ayush Gupta
## Created On: June 25, 2018
## These functions facilitate calculate the inverse of a matrix.  To make
## the calculation more efficient, cacheSolve() will look for a cached inverse
## solution before calculating the inverse again.  mackCacheMatrix() provides
## the functionality to store and retrieve a cached inverse solution to be
## retrieved later.
 
## Write a short comment describing this function
## makeCacheMatrix returns a list with four funtions to use to cache a matrix
## and its inverse.
## set() assigns the matrix.  
## get() returns the matrix.
## getInv() returns the inverse.
## setInv() assigns the matrix inverse.

## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
## testing code 
## matrix <- rbind(c(0, -3, -2), c(1, -4, -2), c(-3, 4, 1))
## inverse <- solve(matrix)
##my_matrix <- makeCacheMatrix(matrix)
##my_matrix$getInv()
## my_inverse <- cacheSolve(my_matrix)
## my_inverse
## my_inverse <- cacheSolve(my_matrix)
## my_inverse
## my_matrix$getInv() 
## matrix2 <- rbind(c(1, 2), c(1, 1))
## inverse2 <- solve(matrix2)
## my_matrix2 <- makeCacheMatrix(matrix2)
## my_matrix2$getInv()
## my_inverse2 <- cacheSolve(my_matrix2)
## my_inverse2
## my_inverse2 <- cacheSolve(my_matrix2)
## my_inverse2
## my_matrix2$getInv()
## my_matrix$getInv()
## my_matrix2$getInv()
