## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    ## storing the cache
    m <- NULL
    ## create the function in the work environnement
    set <- function(y) {
        x <<- y
        ## initializing cache at null
        m <<- NULL
    }
    
    ## get the matrix value
    get <- function() x
    
    ## inverse the matrix and storing in cache 
    setinverse <- function(solve) m <<- solve
    
    ## get inverse matrix that was stored inand initialized to null
    getinverse <- function() m
    ## return the create function in the work environnement
    list(set = set, get = get,
         setinverse = setinverse,
         
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## get inverse matrix kepted in the cache
    m <- x$getinverse()
    ## return inverse matrix if it exist, else create the matrix in the working environnement
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if the matrix does not exist, create it
    data <- x$get()
    m <- solve(data, ...)
    ## setting the inverse matrix in the cache
    x$setinverse(m)
    
    m
}
