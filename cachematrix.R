## The fuctions defined below are used to compute the inverse of
##the matrix and create a cache copy of it to do away with 
##recomputation.

## makecachematrix function is a function that creates a 
##special matrix object and stores a cache of the inverse.
##MakeCacheMatrix is a main function with sub functions such
##as 1.get,2.set,3.getinverse,4.setinverse.
##<<- operator is used to make changes in both sub and main
##functions.

makeCacheMatrix <- function(x = matrix()){
    	m <- NULL
      set <- function(y=matrix()) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) m <<- inv
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## the inverse of the matrix to be returned, is obtained from 
##the cache if it is already computed,else it is computed using 
##solve function.

cacheSolve <- function(x) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}


