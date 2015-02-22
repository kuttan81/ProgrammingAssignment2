## This function returns a list with the below functions
## set - sets the matrix value
## get - gets the matrix value
## getinverse - gets the matrix inverse if it exists
## setinverse - sets the matrix inverse

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


## Function returns the inverse of the input matrix
## It first tries to retrieve the inverse from the cache. If the inverse exists in the cache, the cached value is returned
## If the inverse does not exist, it calculates the inverse, stores it in cache and returns the inverse

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
