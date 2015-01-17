
#Matrix inversion is usually a costly computation so there can be some benefit 
#from caching the inverse of a matrix rather then computing it
#repeatedly.
#This file contains two functions:

#makeCacheMatrix- creates a special "matrix" object that can cache its inverse:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invX <<- inverse
        getinverse <- function() invX
        list(set=set, get=get, 
             setinverse=setinverse,
             getinverse=getinverse)
}


#cacheSolve- computes the inverse matrix however it first checks if the inverse
#has already been computed.If so,it gets the result from the cache and skips 
#the computation.Otherwise, it computes the inverse of the matrix and sets 
#the value of the inverse via setinverse function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getinverse()
        if(!is.null(invX)) {
                message("getting cached data.")
                return(invX)
        }
        data <- x$get()
        invX <- solve(data)
        x$setinverse(invX)
        invX
}
