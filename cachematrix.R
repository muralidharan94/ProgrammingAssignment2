##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than computing it 
##repeatedly. 
##The primary purpose of the functions is that if the contents of a matrix 
##are not changing,  to cache the value of the inverse so that 
##when it is needed again, it can be looked up in the cache rather 
##than being recomputed. This can be done by taking advantage 
##of the scoping rules in R and there ability to preserve there state
##inside an R object.


##The  function, makeCacheMatrix creates a list, which contains a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse

##This function ultimately creates a special "matrix" object 
##that can cache its inverse.

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


##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(a=data,...)
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
