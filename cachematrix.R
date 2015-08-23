## This function will cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) { 
                x <<- y 
                i <<- NULL
        }
        get <- function() x 
        setinverse <- function(inv) i <<- inv 
        getinverse <- function() i
        list(set = set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Work out the inverse of the matrix from the above function. 
#It will reuse the cached result if it is available. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
