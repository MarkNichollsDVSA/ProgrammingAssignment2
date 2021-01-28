## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix creates an object which stores a matrix
## and its inverse
##  
## cacheSolve will return the inverse of an object created 
## with makeCacheMatrix if already cached or calculate and 
## cache the inverse if not

## Write a short comment describing this function
##
## makeCacheMatrix takes as an argument a matrix and will store it,
## the object created will have four functions:
##
## $get() returns the matrix
## $set(x) will change the stored matrix and clear the cache
## $setinv(x) will cache the inverse matrix
## $getinv() returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_mat <<- inv
        getinv <- function() inv_mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## 
## cacheSolve(x) takes an argument (x) an ojbect 
## created with makeCacheMatrix 
## 
## It will return the inverse of the matrix stored in the 
## object (x) first by checking the cached value, if no cached inverse 
## matrix is found, one is calculated, stored, then returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
