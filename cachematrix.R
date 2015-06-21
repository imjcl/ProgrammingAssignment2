## makeCacheMatrix and cacheSolve work together to calculate the inverse of a 
## matrix - using makeCacheMatrix can be used to create a cached matrix
## and if a inverse matrix is calculated using cacheSolve it will be cached.

## makeCacheMatrix takes a matrix (preferrably square), and returns a list
## which stores functions. 
makeCacheMatrix <- function(x = matrix()) {
  ## Initializes an inverse matrix variable, set to NULL.
  inv <- NULL
  
  ## set() allows the input matrix to be changed to a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get() returns the currently set matrix (original input or changed by set())
  get <- function() x
  
  ## setinv() allows the inverse matrix to be set
  setinv = function(solution) inv <<- solution
  
  ## getinv() returns the inverse matrix - stored or otherwise
  getinv = function() inv
  
  ## returns the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns an inverse matrix, of the object where makeCacheMatrix is stored.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  ## Checks to see if a value for inv has been cached, if so, it returns the cached inverse matrix.
  if(!is.null(inv)) {
    message('getting cached solution')
    return(inv)
  }
  
  ## If the value of inv had not been cached, the inverse matrix will be calculated.
  data <- x$get()
  inv <- solve(data, ...)
  
  ## And then the inverse matrix will be cached by setinv().
  x$setinv(inv)
  return(inv)
}
