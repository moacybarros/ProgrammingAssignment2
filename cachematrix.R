##
## Pair functions to compute and cache a matrix and its own inverse.
##

## Creates a "matrix" object which can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    set <- function(m) {
        x <<- m;
        inversedMatrix <<- NULL;
    }
    get <- function() return(x);
    setInverse <- function(inv) inversedMatrix <<- inv;
    getInverse <- function() return(inversedMatrix);
    return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## Create/Return the inverse of the "matrix" produced by `makeCacheMatrix` function. 
## Only calc the inversed atrix if not calculated previously

cacheSolve <- function(m, ...) {
    inversedMatrix <- m$getInverse()
    if(!is.null(inversedMatrix)) {
        message("Cached data being retrieved")
        return(inversedMatrix)
    }
    data <- m$get()
    inversedMatrix <- solve(data, ...)
    m$setInverse(inversedMatrix)
    return(inversedMatrix)
}