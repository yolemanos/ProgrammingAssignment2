## Below are two functions that are used to create a special 
## object that stores an invertible matrix and cache's its inverse


## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to:

## 1- set the values of the matrix
## 2- get the values of the matrix
## 3- set the values of the inverse
## 4- get the values of the inverse


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(Inverse) inv <<- Inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## calculates the inverse matrix of the special "vector" created 
## with the above function. However, it first checks to see if 
## the inverse matrix has already been calculated. If so, 
## it gets the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse matrix in the cache via 
## the setinv function

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
