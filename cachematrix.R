## The below functions takes a matrix, inverses the matrix and stores it in the parent level for use. 
## The function cacheSolve firstly checks if the "getinversematrix" is available. If it is null then
### the function will perform the inversion otherwise it returns the stored inverted matrix


## This function takes a matrix and stores it in the parent level "cache"

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inversematrix) m <<- inversematrix
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
        
        
}


## taking the matrix from "makeCacheMatrix", this function checks to see if the unchanged matrix already exists
## if it exists, then it returns the cached data otherwise it solves for the inversed matrix using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
        
  
}

