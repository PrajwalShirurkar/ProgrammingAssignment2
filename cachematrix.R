
## This function creates a special "matrix" object that can cache its inverse 
## Returns a special "matrix", which is really a list containing a function to

 ##   set the matrix
 ##   get the matrix
 ##   set the inverse of the matrix
 ##   get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	 invertedMatrix <- matrix()
        set <- function(y) {
                x <<- y
                invertedMatrix <<- matrix()
        }
        get <- function() x
        setinv <- function(invMatrix) invertedMatrix <<- invMatrix 
        getinv <- function() invertedMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	  invMatrix <- x$getinv()
        if(!is.na(invMatrix[1,1])) {
                message("getting inverted matrix")
                return(invMatrix)
        }
        dataM <- x$get()
	  ##dataM
        invMatrix <- solve(dataM)
        x$setinv(invMatrix)
        invMatrix
}
