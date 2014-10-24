## This function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse 
    inv <- NULL
    
    ## Set the matrix
    set <- function(matrix){
        x <<- matrix
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    ## Set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## Get the inverse of the matrix
    getinverse <- function() inv
    
    
    ## Return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    
    ## Return inverse if already set
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Get the matrix
    data <- x$get()
    
    ## Use solve() function to get inverse
    inv <- solve(data)
    
    ## Set the inverse
    x$setinverse(inv)
    
    ## Return the inverse matrix
    inv
    
}