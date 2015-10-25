## These functions work together to cache a matrix and its inverse.
## The intention is to avoid computational expense of re-calculating the
## inverse unless the input matrix has changed.

## This function returns a "matrix" which is really a list of functions 
## to set or get the value of the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                    
        x <<- y
        inv <<- NULL
    }
    get <- function() x                     
    setinv <- function(inver) inv <<- inver     
    getinv <- function() inv                    
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function solves for the inverse of the input "matrix" (as created 
## by makeCacheMatrix). The inverse will only be recalculated if the "matrix" 
## has changed.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()               # get the cached inverse of 'x'
        
        # if the cache is not empty proceed to check if 'x' has changed
        if(!is.null(inv) ) {            
            mtrx <- x$get()
            
            # if the product of 'x' and its inverse is the identity matrix
            # 'x' is unchanged so return the cached inverse
            if (all.equal(diag(1,nrow(mtrx),ncol(mtrx)), mtrx %*% inv)){    
                message("getting cached data")
                return(inv)   
            }
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
    
}
