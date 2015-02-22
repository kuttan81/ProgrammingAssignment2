## sets the matrix value
## gets the matrix value
## gets the matrix inverse if it exists
## sets the matrix inverse

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


## Function returns the invesrse of the input matrix
## It first tries to retrieve the inverse from the above function. If the inverse exists, the cached value is returned
## If the inverse does not exist, it calculates the inverse, stores it in cache via the above function and returns the inverse

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
