# These functions create a special matrix object that can cache its inverse.

## makeCacheMatrix takes as its argument a matrix (x) and returns a list
## object containing functions to set and get the original matrix and
## to set and get the matrix's inverse

makeCacheMatrix <- function(x) {
  ## when the function is called for the first time, the inverse cache
  ## should be set to NULL
  inverse <- NULL
  ## the set function allows a cached matrix to be modified later on;
  ## when this happens the inverse cache should be cleared
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## the get function returns the cached matrix
  get <- function() x
  ## the set.inverse caches the matrix inverse when first performed
  ## by the cacheSolve function
  set.inverse <- function(y) inverse <<- y
  ## the get.inverse function returns the cached inverse
  get.inverse <- function() inverse
  ## a list object is returned containing access to all of the above
  ## functions
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## cacheSolve takes as its parameter a CacheMatrix object as created by
## the makeCacheMatrix function and returns the inverse of this matrix

cacheSolve <- function(x) {
  ## check to see if the inverse has already been calculated and cached
  inverse <- x$get.inverse()
  ## if an inverse exists in the cache, go ahead and return it
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  ## if the inverse has not been calculated yet, do so now and store the
  ## inverse in the CacheMatrix object before returning the solution
  data <- x$get()
  inverse <- solve(data)
  x$set.inverse(inverse)
  inverse
}
