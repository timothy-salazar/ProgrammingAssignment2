## These functions make an object containing a matrix and that
## matrix's inverse. If that object is used as an input for the 
## 'cacheSolve' function, it will calculate the inverse of the 
## matrix if there is not already an inverse cached. 

## This function creates a list containing a set of functions. It  
## contains a matrix x, and is able to cache the inverse of a matrix
## which can be retrieved later.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list (set = set, get = get,
              set_inverse = set_inverse, 
              get_inverse = get_inverse)
}


## This function first checks whether a matrix 'x' has an inverse
## cached in our special object. If it is, then it retrieves the 
## inverse from the cache. If it isn't, then it calculates the 
## inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$set_inverse(m)
        m
}