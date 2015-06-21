## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # This represents the cached inverse of a matrix
    inverse <- NULL
    
    # Sets the current value of the matrix and 
    # marks the inverse as unsolved
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Returns the current value of the matrix (x)
    get <- function() {
        return(x)
    }

    # Sets the inverse to a specified value
    setinverse <- function(solution) {
        inverse <<- solution
    }

    # Gets the current value of the inverse (NULL if unsolved)
    getinverse <- function() {
        return(inverse)        
    }

    # Creates an list where each item is a method enclosing this 
    # scope, somewhat like an object.
    object <- list(
        set        = set, 
        get        = get, 
        setinverse = setinverse, 
        getinverse = getinverse
    )
    return(object)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
