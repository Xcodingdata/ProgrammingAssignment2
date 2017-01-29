# Caching the Inverse of a Matrix

makeCacheMatrix <- function (x = matrix()) {
    
    # Make sure the inverse is empty
    inv <- NULL
    
    # Set the matrix
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix   
    get <- function () x
    
    # Calculate the inverse of the matrix   
    setinverse <- function (inverse) inv <<- inverse
    
    # Get the inverse and display as a list   
    getinverse <- function () inv
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is a function that computes the inverse of a matrix returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve function should retrieve the inverse of the matrix from the cache.
# cacheSolve assumes that the matrix is invertible.

cacheSolve <- function (x, ...) {
    
    # Get the inverse
    inv <- x$getinverse()
    
    # Test if a cache is present and if it is notify
    if (!is.null (inv)) {
        message ("getting cached data")
        return (inv)
    }
    
    # If a cache is not present then calculate the inverse and return it    
    data <- x$get()
    inv <- solve (data)
    x$setinverse(inv)
    return (inv)
}


