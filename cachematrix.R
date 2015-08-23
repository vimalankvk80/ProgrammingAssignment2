## Assignment: Caching the inverse of a matrix
## We can calculate inverse for the given square invertible matrix. In order to reduce the 
## the repeated computation same (inverse matrix), we can cache them or future reference by using this <<- symbol.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Setter function in object - sets x value and intialise mean value to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter - get x value
  get <- function() x
  
  # Setter - Set inverse value
  setinverse <- function(inverse) inv <<- inverse
  
  # Getter - Get the inverse value
  getinverse <- function() inv
  
  ## print the signature of each function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. By optional, we may pass other paramters of Solve() functions
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
        
}
