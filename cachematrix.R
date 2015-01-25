## Code for solving the inverse of a given matrix
## The inversed value will be cached by subsequent calls

## Initialize a matrix environment 
makeCacheMatrix <- function(x = matrix()) {
        # the inverse is set to a null value
        inverse <- NULL
        
        # set a new matrix
        set <- function(y) {
                x <<- y
                # the inverse of a new matrix beigns as null
                inverse <<- NULL
        }
        
        # function that gets the loaded matrix
        get <- function() x
        
        # save the inverse of the loaded matrix
        setinv <- function(inv) inverse <<- inv
        
        # get the inverse matrix
        getinv <- function() inverse
        
        # return subfunctions of environment when initialized
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function that gets a matrix and returns its inverse
## If the value is cached, it will use it to save time. If it is not cached, 
## it will make the inverse matrix and save it to the cache
cacheSolve <- function(x, ...) {
        # get the inverse from the cache to find later if it exists
        inverse <- x$getinv()
        
        # if the inverse is in the cache, return it from there
        if(!is.null(inverse)) {
                message("Matrix inverse found in cache.")
                return(inverse)
        }
        
        # if not, calculate the inverse matrix and...
        message("Matrix inverse not found in the cache. Calculating it...")
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        
        # ...and save it in the cache
        x$setinv(inverse)
        
        # return the inverse matrix
        inverse
}
