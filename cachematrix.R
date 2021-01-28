## Define methods to enable to caching of a matrix with it's inverse
## this should enable us to reduce the need to calculate the inverse
## repeatedly.

## Define a cache matrix that enables the caching of a matrix and its
## inverse to eliminate the need to repeatedly calculate the inverse.
## Defines set and get functions for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse = getinverse)
}

## This function will attempt to retrieve the matrix inverse from the 
## cacheMatrix.  If the inverse is not yet set or has been removed due 
## to a change to the underlying matrix it will compute the inversee.
## Returns the inverse of the matrix provided as x.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()    
    if(!is.null(inv)){
        return(inv)        
    }
    my_matrix <-x$get()
    inv <- solve(my_matrix)
    x$setinverse(inv)
    inv
}
