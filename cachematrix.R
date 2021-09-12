## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                      ## set the inv as NULL
        set <- function(y) {                    
                x <<- y                   ## define a function to set the matrix to another variable          
                inv <<- NULL                        
        }
        get <- function() x               ## define a function to get the value of the matrix   
        
        setinverse <- function(inverse) inv <<- inverse  ## define a function to set inv to another variable
        getinverse <- function() inv       ## get the inverse              
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## it is a list which contains the functions we set above

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()        ## use the function above
        if(!is.null(inv)) {          ## if so
                message("getting cached data")
                return(inv)
        }
        data <- x$get()             ## if not
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
