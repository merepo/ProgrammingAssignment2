## This R-markdown file contains two functions.
## One function serves to cache the inverse of a matrix
## The second function serves to calculate the inverse of a matrix and store it
## in the cache
## These functions are useful as matrix inversion is a taxing computation. By 
## using these functions, repetitive computations could be avoided thus 
## improving overall efficiency.



## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The following function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. It first checks if the inverse has already been computed.
## If the inverse has already been calculated  then the function should retrieve 
## the inverse from the cache. 
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}



