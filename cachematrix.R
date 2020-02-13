## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver = NULL
        set <- function(y){      ## set the matrix created
                x <<- y
                inver <<- NULL
        }
        get <- function()x       ## get the matrix created
        setinversion <- function(inversion) inver <<- inversion  ## set the inversion of the matrix
        getinversion <- function() inver  ## get the inversion of the matrix
        list(set = set,
             get = get, 
             setinversion = setinversion, 
             getinversion = getinversion)  ## return the the list of four functions defined above
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inver <- x$getinversion()
        if(!is.null(inver)){           ## test whether the inversion has been calculated and cached
                message("getting cached matrix")
                return(inver)
        }
        matrix <- x$get()         ## if the inversion has not been cached, do the calculation
        inver <- solve(matrix, ...)
        x$setinversion(inver)
        inver   ## Return a matrix that is the inverse of 'x'
}
