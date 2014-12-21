## The first function, `makeCacheMatrix` creates a special Matrix, which is really a 
## a list containing a function to
## 1. Set the value of the Matrix
## 2. Get the value of the matrix
## 3. Set the value of the Inverse Matrix
## 4. Get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
	    INV <- NULL
            set <- function(y) {
                    x <<- y
                    INV <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) INV <<- solve
            getinverse <- function() INV
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## This function calculates the inverse of the special "matrix"
## created with the above function. It first checks whether the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache using the setinverse 
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            INV <- x$getinverse()
            if(!is.null(INV)) {
                    message("getting cached data")
                    return(INV)
            }
            data <- x$get()
            INV <- solve(data, ...)
            x$setinverse(INV)
            INV
}
