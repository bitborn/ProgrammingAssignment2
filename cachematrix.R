##
## Takes a matrix and returns a list of functions enclosing the function's 
## scope. These functions can be used to get and set various data in
## the environment, most importantly `inverse` the cached result of
## solving the matrix.
##
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


##
## Returns the inverse of a "cache matrix".
## It uses the functions defined in `makeCacheMatrix` to either
## get the matrix's cached inverse, or calculate it, if it 
## hasn't been already.
##
cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()

    # If we have the value, just return it...
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    # If we don't, calculate it.
    inverse <- solve(x$get(), ...) 

    # Remember to set it before returning
    x$setinverse(inverse)
 
    return(inverse)  
}
