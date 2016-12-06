## The function makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(maty)
        {
                x <<- maty
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
