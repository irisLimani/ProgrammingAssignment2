## Caching the Inverse of a Matrix
## To compute the inverse of a matrix the following two functions has been used

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
        m = NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cachesolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}

