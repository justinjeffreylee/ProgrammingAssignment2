## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes in a matrix, uses the solve() to invert it,
## and stores the result.  It also stores the original matrix.

## cacheSolve takes the new matrix and compares it to the original
## matrix stored in makeCacheMatrix.  If they are the same (ie the 
## matrix has not changed), then it checks to see if the inverse
## matrix value has been stored.  If so it returns that results, 
## otherwise it calculates the inverse, and stores and returns the value.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        originalmatrix <- x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             originalmatrix = originalmatrix)        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        newmatrix <- x
        originalmatrix <- x$originalmatrix
        ## check to see if matrix has changed since result was cached
        if(originalmatrix == newmatrix){
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
        }
        ## if matrix has changed, or result is not cached, calculates and caches the result
        data <- x$get()
        m <- mean(data, ...)
        x$setinverse(m)
        m
}
