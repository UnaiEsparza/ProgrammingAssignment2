## makeCacheMatrix and cacheSolve functions cache the inverse of a matrix to
## avoid computing it repeatedly.



## makeCacheMatrix creates a special vector containing the input data for
## cacheSolve. It's actually a list containing four functions: set, get, 
## sentinv and getinv.

makeCacheMatrix <- function(a = matrix()) {
        inv <- NULL
        set <- function(b) {
                a <<- b
                inv <<- NULL
        }
        get <- function() a
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve gets the inverse of the matrix using the solve() function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinv function.

cacheSolve <- function(a, ...) {
        inv <- a$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- a$get()
        inv <- solve(data, ...)
        a$setinv(inv)
        inv
}

