## Description: Compute the inverse of a square matrix and cache the result
## so that a prior result will return the cached value instead of recomputing it

## makeCacheMatrix creates the set of functions (set, get, setinv, getinv)
## available to operate on the 'special' matrix with caching capability
## to store the matrix and its inverse when cacheSolve is first called (see below)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL								# mean is null
        set <- function(y) {					# set value of matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x 					# get value of vector
        setinv <- function(solvex) inv <<- solvex # set inverse function
        getinv <- function() inv				# get inverse function
        list(set = set, get = get,				# return list of functions
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve uses our special matrix. If there is a cached result, it returns it.
## Otherwise, it gets the input matrix, calculates the inverse, and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()		# inverse stored already?
        if(!is.null(inv)) {		# yes, return it
                message("getting cached data")
                return(inv)
        }
        data <- x$get()			# no, get data from x
        inv <- solve(data, ...)	# calculate the inverse for that data
        x$setinv(inv)			# store the inverse
        inv	
}
