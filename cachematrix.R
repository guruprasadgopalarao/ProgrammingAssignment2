
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
    ### Method to retrieve matrix
    get <- function() {
    	### Return the matrix
    	matx
    }
    ### Method to set inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ### Method to get inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    ### Return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

### 2. Function "cacheSolve": This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ### Return inverse matrix of 'x'
    matx <- x$getInverse()
    ### Return inverse matrix, if its already set
    if( !is.null(matx) ) {
            message("Retrieve Cached Data")
            return(matx)
    }
    ### Get matrix from defined object
    data <- x$get()
    ### Calculate its inverse
    matx <- solve(data) %*% data
    ### Set inverse to the defined object
    x$setInverse(matx)
    ### Return matrix
    matx
}
