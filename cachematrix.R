## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as input and returns a list that contains
## special functions to set and cache its value and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Let i represent the cached inverse of this matrix, we set it empty to start
  i <- NULL
  
  ## define the 'set' function - simply set the matrix value and clear the inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## defines the 'get' function - simply returns the matrix
  get <- function() x
  
  ## defines the set_inverse function, we simply store the inverse value
  set_inverse <- function(inverse) i <<- inverse
  
  ## and return it when necessary
  get_inverse <- function() i
  
  ## we return a list of functions that can operate on this matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function will return the inverse of a given matrix, by
## looking up its cached value and if it doesn't exist, by 
## using the solve() function and then assigning the returned 
## value to the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  
  ## We already have the inverse cached, just return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse
  i <- solve(data)
  
  ## Store it in the cache
  x$set_inverse(i)
  
  ## And return it
  i
}
