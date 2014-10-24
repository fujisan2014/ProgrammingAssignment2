# The following 2 functions are used to cache the inverse of a matrix.
 

# makeCacheMatrix : This function creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inversecache <- NULL
    set <- function(y)  {
        x <<- y
        inversecache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversecache <<- inverse
    getinverse <- function() inversecache
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    }


# CacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
     inversecache <- x$getinverse()
     if(!is.null(inversecache)) {
         message("getting cached inverse data.")
         return(inversecache)

     }
     data <- x$get()
     inversecache <- solve(data)
     x$setinverse(inversecache)
     inversecache
    }
