## This is assignment 3 code for Coursera Data Science: R Programming 
## GitHub user: ZachVanYsseldyk

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode = "matrix"
    invers <- NULL                          ## initialize invers as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        invers <<- NULL                     ## if there is a new matrix, reset invers to NULL
    }
    get <- function() x                     ## define the get function - returns value of the matrix argument
    
    setinverse <- function(inverse) invers <<- inverse  ## assigns value of invers in parent environment
    getinverse <- function() invers                     ## gets the value of invers where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse already was calculated (and the matrix has not changed),
## then cacheSolve will get the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invers <- x$getinverse()
    if(!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
    data <- x$get()
    invers <- solve(data, ...)
    x$setinverse(invers)
    invers
}
