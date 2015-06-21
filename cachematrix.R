## The following two functions will store a matrix and 
## cache the inverse of a matrix
## Author: Eunice Park

## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.  4 functions perform:
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
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse=getinverse)
}


##  cacheSolve(matrix, ...) calculates the inverse of a matrix created with
## the above function.  It first checks to see if the inverse has already been
## calculated.  If so, it gets the inverse from the cache and skips the 
## computation.  Otherwise, it calculates the inverse of the data and sets
## the value in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message('getting cached data')
                return (m)
        } 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
