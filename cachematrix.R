## This function creates a list containing a function to set value of matrix,
## get value of matrix, and the same with the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL							
	set <- function(y) {					
		x <<- y						#Sets x within function environment
		inv <<- NULL					#Sets inv within function environment
	}
	get <- function() x					
	setinverse <- function(inverse) inv <<- inverse	#Sets the inverse of the matrix
	getinverse <- function() inv 				#Gets the inverse of the matrix 
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse) 
}


## This function looks up the cache to find out if the inverse is already
## computed, and if not, it computes the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()					#checks if inverse is computed
	if(!is.null(inv)) { 					#if so, getting cached data
		message("getting cached data")		
		return(inv) 
	}
	data <- x$get()						#gets data
	inv <- solve(data, ...)					#solves for inverse
	x$setinverse(inv)						#sets inverse
	inv								#prints inverse
}
