## A cominbation of functions that takes a matrix object argument, creates a list of 
## fuctions set, get, create an inverse and return an inverse of a matrix. The first time ## a matrix is passed the inverse is computed and stored in cache. If the same matrix is ## passed to be inversed again the cached value is returned

## makeCacheMatrix is passed a matirx argument and returns a list containing functions to 
## set, get, inverse and return an inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of what it is passed, its computed the ## first time round, if passed again its returned from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}