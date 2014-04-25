## Functions:
## makeCacheMatrix - creates a special matrix object that can
## cache its inverse
## cacheSolve - computes the inverse of the special matrix returned
## by makeCacheMatrix

## This function receives a matrix from the user and creates a
## special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##Initially there's no inverse matrix
    inv <- NULL
    
    ##Set the matrix to matrix "y" passed by the user
    ##and the inverse to NULL too
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    ##Get the matrix
    get <- function() x
    
    ##Set the inverse matrix passed as matrix "inverse"
    ##passed by the user
    setinverse <- function(inverse) inv <<- inverse
    
    ##Get the inverse matrix
    getinverse <- function() inv
    
    ##Create the list to return containing all
    ##the methods above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Get the inverse from x object
    inv <- x$getinverse()
    
    ##Verify if there is a stored value from the inverse and,
    ##in this case, return it
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ##Else, computes the inverse of x with the function solve
    mat <-x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    
    ##Return the inverse
    inv
}