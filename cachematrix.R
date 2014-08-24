## This program returns the inverse of a matrix using two functions:
## 1) makeCacheMatrix(), a help function that creates setters and getters for the original matrix and its inverse.
## 2) cacheSolve() that performs the calculations to obtain the inverse.

## This function creates setters and getters for a matrix 'x' and its inverse 'inv'.
makeCacheMatrix <- function(x = matrix()) {
	## This sets the inverse equal to NULL.
	inv <- NULL
	## This sets x equal to y and, again, the inverse equal to NULL.
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	## This gets x.
	get <- function() x
	## This sets 'inv' equal to 'inverse'.
	setinverse <- function(inverse) inv <<- inverse
	## This gets 'inv'.
	getinverse <- function() inv
	## This creates a list with all the functions.
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a matrix 'z' using the getters and setters of the makeCacheMatrix() function
cacheSolve <- function(z) {	
        ## This retrieves the function makeCacheMatrix() and puts all functions in 'x'
        x <- makeCacheMatrix(z)
        ## This and the if condition after checks whether it is already an inverse in the cache.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }        
        # Gets the matrix 'z' and puts it in 'data'.
        data <- x$get()        
         ## Returns a matrix 'inv' that is the inverse of 'z'
        inv <- solve(data)        
        ## Puts the solution into 'inv' 
        x$setinverse(inv)       
        ## Returns 'inv', the inverse of 'z'
        inv
}
