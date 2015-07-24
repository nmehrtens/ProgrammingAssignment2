## These pair of functions are used to create an object
## that stores a matrix and cache's its inverse.

## Usage:
## First parse an invertible matrix to the makeCacheMatrix function, e.g.
## > m <- matrix(c(1,4,5,4,5,6,7,8,9),3,3)
## > tempM <- makeCacheMatrix(m)
## Then solve for the inverse matrix using cacheSolve, e.g.
## > cacheSolve(tempMatrix)
## Running the above command again will retrieve, rather than calculate, the inverse.


## makeCacheMatrix function creates a 'matrix' object that caches its inverse.
## The object contains a list to:
## set the value of a matrix; get the value of a matrix;
## set the value if its inverse; get the value of its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL				# 'inv' = stored value for the inverse matrix.
	       					# initializing stored value for inverse matrix, 'inv', to NULL

	setmatrix <- function(y) {		# Function that sets the matrix stored in cache and resets the inverse matrix
                x <<- y		 		# updates the matrix stored in cache, 'x'.
                inv <<- NULL			# re-initilizes inverse matrix value to NULL
		    				# (as the matrix has been updated and any previous inverse matrix value is no longer valid). 
        }
	
        getmatrix <- function() x		# function that retrieves the matrix stored in cache.
	
        setinv <- function(solve) inv <<- solve	# function that calculates the inverse matrix of the stored matrix, and stores that inverse matrix in cache
        getinv <- function() inv    		# function that retrieves the inverse matrix in cache
	
        list(setmatrix = setmatrix,
	     getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve returns the inverse matrix of 'x'.
## It first checks to see if the inverse is already cached.
## If not, it calculates and stores the inverse matrix using the makeCacheMatrix function.


cacheSolve <- function(x, ...) {

	inv <- x$getinv()               ## retrieves cached value for the inverse of matrix 'x' using the 'getinv' function.
	
        if(!is.null(inv)) {             ## if a value for the inverse matrix is retrieved, it returns the inverse matrix
			  		## and breaks out of the cacheSolve function. 
                message("getting inverse matrix")
                return(inv)
        }
	
        data <- x$getmatrix()           ## If an inverse matrix is not retrived, i.e. its value is NULL, it retrieves the
	     				## cached data for the matrix x using the 'getmatrix' function.
					
        inv <- solve(data, ...)         ## Then calculates the inverse matrix of stored matrix data using the solve function.
        x$setinv(inv)	   		## The value for the inverse matrix is then stored in cache using the 'setinv' function
        inv				## And finally returns the inverse of the matrix 'x'
}


