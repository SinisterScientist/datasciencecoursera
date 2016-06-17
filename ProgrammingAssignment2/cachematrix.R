## These functions solve for the inverse of a matrix. The inverse
## of the matrix is cached, if this is the first solution to it.
## Otherwise, the cached solution is retrieved. This minimizes
## the required computations.
## To use: assign the result of makeCacheIndex to a variable.
## Pass that variable to cacheSolve

## makeCacheMatrix creates a special matrix, which is 
## a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve solves for the inverse of a matrix created with makeCacheMatrix
## It first checks to see if the inverse has been solved for, and if so, it
## gets the inverse from the cache. If not, it solves for the matrix inverse,
## and sets the value in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
