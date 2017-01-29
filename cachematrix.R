# Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse by
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function (x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function (x)
        setinverse <- function (inverse) inv <<- inverse
    getinverse <- function () inv
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is a function that computes the inverse of a matrix returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve function should retrieve the inverse of the matrix from the cache.
# cacheSolve assumes that the matrix is invertible.

cacheSolve <- function (x, ...) {
    inv <- x$getinverse()
    if (!is.null (inv)) {
        message ("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve (data)
    x$setinverse(inv)
    inv
}
