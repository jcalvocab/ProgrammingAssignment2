## DESCRIPTION:
## makeCacheMatrix: This function creates a special "matrix" object 
## (a list containing functions to set/get the matrix and get/set the inverse) 
## that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" object 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 
##       1. set the value of the matrix
##       2. get the value of the matrix
##       3. set the value of the inverse of the matrix
##       4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object returned by 
## makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inversed matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
