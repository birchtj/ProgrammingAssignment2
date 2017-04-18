
## cache the inverse of a matrix

## create a matrix object that can cache the inverse of itself

makeCacheMatrix <- function(x = matrix()) {
        
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setInvert <- function(inverse) invert <<- inverse
        getInvert <- function() invert
        list(set = set,
             get = get,
             setInvert = setInvert,
             getInvert = getInvert)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        invert <- x$getInvert()
        if (!is.null(invert)) {
                return(invert)
        }
        mat <- x$get()
        invert <- solve(mat, ...)
        x$setInvert(invert)
        invert
}
