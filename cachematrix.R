## R Programming - Programming Assessment 2
## The functions below cache the computation of the inverse of a matrix
        
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { 
                x <<- matrix(y)
                inv <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from 
##the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #get cached data
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get() # if cache not found, compute
        inv<- solve(data, ...)
        x$setinverse(inv)
        inv    
}