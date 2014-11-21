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
        # Create the 'set' function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Create the 'get' function
        get <- function() x
        # Create the 'getinverse' function
        setinverse <- function(inverse) inv <<- inverse
        # Create the 'getinverse' function
        getinverse <- function() inv
        ## Return a list of functions
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
        ## Get the matrix 
        tmpMatrix<-x$get()
        
        ## NOTE: If the matrix has changed (with the 'set() function of 
        ## the special 'matrix' object) we set the variable 'inv' to NULL, 
        ## so we only need check is 'inv' is NULL
        
        ## Check if the inverse has already been calculated
        if (!is.null(inv)) {
                message("getting cached inversed matrix")
                return(inv)
        }
        # Get the original matrix
        data <- x$get()
        # Calculte the inverse
        inv <- solve(data, ...)
        # Set the inverse in the special 'matrix' object
        x$setinverse(inv)
        # Return the inverse
        inv
}
