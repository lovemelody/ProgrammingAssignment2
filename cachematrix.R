## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Suppose i = inverse of matrix x
## makecacheMatrix will return a list containing functions likes: 
##set (store in cache, x), get(return x), setinverse (store in cache, inverse of x), getinverse(return inverse of x)

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)       

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()             #query the x vector's cache
                  
        if(!is.null(i)) {               #if there is a cache
                message("getting cached data") 
                return(i)               #just return the cache, no computation needed
        }
        
        data <- x$get()                 #if there's no cache
        i <- solve(data, ...)           #we actually compute them here
        x$setinverse(i)                 #save the result back to x's cache
        i   
}
