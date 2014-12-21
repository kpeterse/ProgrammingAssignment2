## These functions create a matrix object given dimensions by the user
## For the given object cacheSolve calculates the inverse of the object
## and caches the result for reference.  Operators on the object include 
## "get," "set," "getinverse" and "setinverse"

## takes provided matrix and creates an object with functions allowing the 
## user to cache the object and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## calculates the inverse on the matrix object and pushes the result to the 
## object

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

