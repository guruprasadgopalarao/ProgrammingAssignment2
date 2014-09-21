
###  R Programming: Assignment : Caching the Inverse of a Matrix

### Assignment is to write a pair of functions that cache the inverse of a matrix

### 1. Function "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( matx = matrix() ) {

    ### Initialize the inverse property
    i <- NULL

    ### Method to set the matrix
    set <- function( matrix ) {
            matx <<- matrix
            i <<- NULL
    }

    ### Method the get the matrix
    get <- function() {
    	### Return the matrix
    	matx
    }

    ### Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ### Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ### Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

### 2. Function "cacheSolve": This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

    ### Return a matrix that is the inverse of 'x'
    matx <- x$getInverse()

    ### Just return the inverse if its already set
    if( !is.null(matx) ) {
            message("getting cached data")
            return(matx)
    }

    ### Get the matrix from our object
    data <- x$get()

    ### Calculate the inverse using matrix multiplication
    matx <- solve(data) %*% data

    ### Set the inverse to the object
    x$setInverse(matx)

    ### Return the matrix
    matx
}
