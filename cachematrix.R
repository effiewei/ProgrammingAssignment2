## Function makeCacheMatrix and cacheSolve work together to cache
## the inverse of a matrix.

## Function makeCacheMatrix creates a special "vector", which is 
## really a list containing a function to set & get the matrix and
## set and get the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)

}


## Function cacheSolve first checks to see if the inverse has already
## been calculated. If so, it gets the inverse and return it. Otherwise,
## it calculates the inverse of the matrix using solve function and 
## sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <-x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
