## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Cache Matrix Function 
##
## The first function, makeCacheMatrix  creates a special "matrix", which 
## cache its inverse
##
##1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the inverse of the matrix
##4) get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Cache Solve Function
## 
## The following function calculate the inverse of the special "matrix" 
## calculated with the above function. It checks first to see if the inverse 
## have already been calculated. If yes, it gets the inverse from the cache and 
## skip the calculation (which can be costly in resources if we are dealing with 
## a large matrix).
## 
## Otherwise it calculates the inverse of the matrix and set the inverted matrix
## in the cache using the  setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <= x$getinverse()

		## check if the matrix has already being inverted. 
    	## If this is the case print a message and return the inverted matrix. 
		if(!is.null(inver)) {
				message("returning cached matrix... ")
				return(inver)
		}
		
		## Otherwise, calcultate the inverse using the setinverse function
		matx <- x$get()
		inver <- solve(matx)
		x$setinverse(inver)
		inver
}