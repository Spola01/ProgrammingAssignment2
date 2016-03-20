## These functions create an inverse of a matrix that is used as an input
 

## This function creates a special matrix list that contains the matrix
## and the inverse if already calculated

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
           x <<- y
           inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function checks to see if an inverse has been previously calculated for a given special matrix
## and returns it or if it doesnt exist calculates it


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("matrix inverse already solved, getting cached calculation")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}