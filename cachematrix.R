## Brent MacArthur
## Assignment 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        #  <<- causes a search to be made through parent environments for 
        #      an existing definition of the variable being assigned.
        x <<- y            
        inv <<- NULL
    }
    get <- function() x   # A function with no arguments that returns the value stored in x
    
    # Take the "inverse" argument from the calling function (cacheSoleve) and store it in inv
    setInverse <- function(inverse) inv <<- inverse 
    
    getInverse <- function() inv
    
    # "Store" the values
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting inverted matrix")
        return(inv)
    }
    
    m <- x$get()
    inv <- solve(m, ...)
    x$setInverse(inv)
    inv
}
