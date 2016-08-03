## R functions for computing the inverse of a square matrix
	## and caching the inverse to avoid future recalculation had the matrix
	## not changed
	

	## This function creates a special "matrix" object that can cache its inverse.
	

	makeCacheMatrix <- function(x = matrix()) {
	  # in
	  inv_mat <- NULL 
	  set <- function(y) {
	    x <<- y
	    inv_mat <<- NULL
	  }
	  
	  get <- function() x
	  setinverse <- function(inverse) inv_mat <<- inverse
	  getinverse <- function() inv_mat
	  # return the list of all functions
	  list(set = set, get = get,
	       setinverse = setinverse,
	       getinverse = getinverse)
	  
	}
	

	

	## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
	##  above. If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.
	

	cacheSolve <- function(x, ...) {
	  ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinverse()
	  # checking already calculated in verse and returning if so
	  if(!is.null(inv)) {
	    message("getting cached data")
	    return(inv)
	  }
	  
	  # calculating the inverse if not calculated already or the matrix has changed (was set)
	  data <- x$get()
	  inv <- solve(data, ...)
	  x$setinverse(inv)
	  inv
	  
	}

