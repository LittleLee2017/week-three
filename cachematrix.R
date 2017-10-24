## This pair of functions are used to compute the inverse of a matrix and remember it.
## it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. 

## The first function creates a list for the matrix whose inverse you want to know
## For example, "mm <- makeCacheMatrix(matrix(1:4, 2, 2))"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
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


## The seconed functions takes the list created above and compute the inverse.
## For example, type "cacheSolve(mm)", then we get the inverse.
## Use "mm$setinverse" to check that the inverse has been stored after the computation.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

