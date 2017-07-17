## makeCacheMatrix and cacheSolve cache the inverse of a matrix
## to avoid repeating costly computations.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) { ## input X is an invertible matrix.
        M <- NULL
        set <- function(Y) { ## sets the value of the matrix.
                X <<- Y
                M <<- NULL
        }
        get <- function() X ## gets the value of the matrix
        setinv <- function(inv) M <<- inv  ## sets the value of the inverse
        getinv <- function() M ## gets the value of the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        M <- X$getinv()
        if(!is.null(M)){ ##if inverse is already calculated, returns cached
                         ## matrix from previous function
                message("getting cached data")
                return(M)
        }
         data <- X$get()
         M <- solve(X)
         X$setinv(M)
         M
}
