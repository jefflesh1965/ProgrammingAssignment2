## The combination of these two functions will create matrix object as a
##cache and will take the inverse of that cached matrix object


## This function creates a matrix object that will take the inverse of the 
##matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y)  {
                x <<- y
                inverse <<- NULL
        }
        get <- function () x
        setInverse <- function(solveMatrix)  inverse <<- solveMatrix
        getInverse <- function() inverse
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}



## This function solves the inverse of the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
