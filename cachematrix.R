## The following two functions will store a matrix and 
## cache the inverse of a matrix
## Author: Eunice Park

## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse value.  4 functions are performed and saved in a makeCacheMatrix list:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## The following cacheSolve(matrix, ...) calculates the inverse of a matrix
## created with makeCacheMatrix().  It first checks to see if the inverse has 
## already been calculated.  If so, it gets the inverse from the cache & skips
## the computation.  Otherwise, it calculates the inverse of the data and sets
## the value in the cache via the setinverse function
##
## Test data:   cacheSolve(makeCacheMatrix(matrix(c(4,2,7,6),2,2)))
## Result:      [,1] [,2]
##         [1,]  0.6 -0.7
##         [2,] -0.2  0.4

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if (!is.null(m)) {
                message('getting cached data')
                return (m)
        } 
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
