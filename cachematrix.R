## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and 
## these two functions cache the inverse of a matrix 
## rather than compute it repeatedly 

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse
## this object can execute four methods:
## set -> cache the matrix to be inverted
## get -> retrieve the matrix to be inverted
## setinverse -> cache the inverted matrix 
## getinverse -> retrieve the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
	## this is the set function that assigns to tha variable the argumet passed matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
	## This is the get function that returns the cached matrix
    get <- function() x
	## this is the funcion that caches the inverse of the matrix
    setinverse <- function(inversa) m <<- inversa
	## this is the funcion that returns the cached inverse of the matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
## The function checks if a matrix inverse has already been cached 
## otherwise first gets the matrix and then computes the inverse
cacheSolve <- function(x, ...) {
    #checks to see if a matrix has been cached already
    inversa <- x$getinverse()
    if(!is.null(inversa)) {
        message("getting cached data")
        return(inversa)
    }
	#If not cached I get the original matrix and compute the inverse
    data <- x$get()
    inversa <- solve(data, ...)
	## cache the inverse of matrix so it ca be retrieved later
    x$setinverse(inversa)
    inversa
}
