## This is my code for programming assignment 2 in the R Programming course
## The two functions defined in this R script make use of the '<<-' operator
## in order to cach the inverse of a given matrix 

## The function 'makeCacheMatrix' creates a special 'matrix'
## This function returns a list of functions to access and manipulate matrices 
makeCacheMatrix <- function( X = matrix() ) {
    inverse = NULL 
    
    set <- function( Y ) {
        X <<- Y
        inverse = NULL
    } 
    
    get <- function() X
    
    setinv <- function( inv ) inverse <<- inv 
    getinv <- function() inverse
    
    list( set = set, get = get,
          setinv = setinv,
          getinv = getinv )
}

## The function cacheSolve returns the inverse of a special matrix  
## if the inverse is already computed, return the cached value
## else, compute the inverse and return it
cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'
    inv <- X$getinv()
    
    if( !is.null( inv ) ) {
        message("Getting cached data")
        return( inv )
    }
    # print('I am here! :D')
    data <- X$get()
    inv <- solve(data)
    X$setinv( inv )
    inv
}
