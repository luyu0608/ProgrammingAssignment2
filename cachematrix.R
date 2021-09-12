## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
        inv <- NULL                        ## set the inverse as NUll     
        set <- function(y) {               ## define a function called set which sets the matrix to another variable  
                x <<- y                             
                inv <<- NULL                        
        }
        get <- function() x                ## define a function to get the matrix
       
        
        setinverse <- function(inverse) inv <<- inverse   ## define a function to set the inv to another variable
        getinverse <- function() inv                     ## if the inverse has been calculated before, this function will return it
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## set a list to call the functions above


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {     
        inv <- x$getinverse()  ##call the getinverse() to check whether the inverse has been calculated before
        if(!is.null(inv)) {
                message("getting cached data"). ## if the inverse has been calculated
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)       # if not   
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
