## The following functions serve to demonstrate how memory usage can be
## more efficient than processing power.  By storing the results in
## memory it is far faster to pull than to reprocess every time the
## results of said process need to be run.



## MakeCacheMatrix sets up the initial special use matrix for this set of functions.
## this function doesn't invert the matrix.  It simply sets the matrix up to be
## inverted.

makeCacheMatrix <- function(x = matrix()) {
    ## for the purposes of this exercise, this function should only be
    ## run once.  as such inv will be sewt to NULL the first pass
    ## and then set to the actual inverted matrix through calls later
    inv <- NULL
    
    ## set is the function that sets the special matrix we are making
    ## as the basic matrix we are testing, again setting inv to NULL
    ## as it should be assumed that when calling this function we
    ## are dealing with a fresh, uintested matrix.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get calls the un-inverted matrix back.
    get <- function() x
    
    ## setinverse does exactly what it sounds like.  Within the object; inv is set
    ## to the inverted matrix as called.
    setinverse <- function(inverse) inv <<- inverse
    
    ## getinverse is the function that does exactly what it says.  It calls the
    ## inverse from memory
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is the heavy lifting function of the series.  This is the
## function that actually serves to both check if the matrix has already
## been inverted, and if if already has, pull the results from memory.
## If it hasn't been inverted already, it is also the function that
## does the actual inverting, and stores the results in memory.

cacheSolve <- function(x, ...) {
    
    ## this calls the inverted matrix from the object "x" being called
    inv <- x$getinverse()
    
    ## if the called inverted matrix is NOT null, that means it has been calculated,
    ## and as such can suimply be called from memory.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if the above function should test false, and "inv" is NULL, we go about
    ## calculating the inverse function and storing the results in memory
    data <- x$get()  ## pulls the basic matrix from the object in memory
    inv <- solve(data)  ## inverts the matrix using the "solve" function
    x$setinverse(inv)  ## Sets the object "x"s internal inverse as the inverse as calculated above
    inv ## Returns said inverse
}