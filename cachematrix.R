## A pair of functions that cache the inverse of a matrix
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize the inverse property
  i <- NULL
    ## Method to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Method the get the matrix  
  get <- function() x ## Return the matrix  
  
  ## Method to set the inverse of the matrix
  setinverse <- function(inv) i <<- inv
  
  ## Method to get the inverse of the matrix    
  getinverse <- function() i
  
  ## Return a list of the methods
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )

}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## Return the inverse if its already set
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Get the matrix from our object 
  data <- x$get()
  ## Calculate the inverse using matrix multiplication  
  i <- inv(data, ...)
  ## Set the inverse to the object  
  
  x$setinverse(i)
  ## Return the matrix   
  i 
}