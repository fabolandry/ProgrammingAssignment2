##The following two functions are used to calculate the inverse of a matrix, 
##if the matrix has been cached before, 
##the inverse of a matrix will be returned right now rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {

		#begins by setting the mean to NULL as a placeholder for a future value

		m <- NULL


		
		#defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL

		set <- function(y){

					
					x <<- y

					m <<- NULL

		}

		
		#returns the vector, x

		get <- function() x


		#sets the inverse, m, to "inverse"

		setinverse <- function(inverse) m <<- inverse


		#returns the inverse, m

		getinverse <- function() m


		#returns the 'special vector' containing all of the functions just defined

		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}




cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

		m <- x$getinverse() # if an inverse has already been calculated this gets it


		if(!is.null(m)) {# check to see if cacheSolve has been run before

		message("getting cached data") # if yes display a msg

		return(m)

		}


		#if the inverse iS not there, first it is calculated and then retrieved.

		data <- x$get()

		m <- solve(data, ...)

		x$setinverse(m)

		m

	}