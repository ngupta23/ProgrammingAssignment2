## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) invmat <<- inv
  getinverse <- function() invmat
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()
  if (!is.null(invmat)){
    message("getting cashed data")
    return(invmat)
  }
  
  mat <- x$get
  invmat <- solve(mat, ...)
  x$setinverse(invmat)
  invmat
  
}
