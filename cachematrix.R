
# The makeCacheMatrix creates a special "vector", 
# which is really a list containing a function to
#
# set the matrix
# get the matrix
# set the invere the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  invMat <- NULL
  # Setting the Matrix
  set <- function(y){
    x <<- y
    invMat <<- NULL
  }
  # Returns the matrix
  get <- function() x
  
  # Sets the inverse of the matrix
  setInverse <- function(inv) invMat <<- inv
  # Gets
  getInverse <- function() invMat
  
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Will returned cached value of the inverse if it exists
## Otherwise it will find the inverse and cache it as well

cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()
  if (!is.null(invMat)){
    message("getting cashed data")
    return(invMat)
  }
  
  mat <- x$get
  invMat <- solve(mat, ...)
  x$setInverse(invMat)
  invMat
  
}
