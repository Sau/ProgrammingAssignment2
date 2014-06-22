## rprog-004 assignment 2: lexical scoping
## contains functions to calculate inverse of matrix
## and cache the inverse matrix for future use

## @function makeCacheMatrix
## @argument x = matrix() 
## assumption - supplied argument is invertible matrix
## creates global matrix from argument

makeCacheMatrix <- function(x = matrix()) {
  #make copy
  copyxmatrix <<- x
  x
}


## @function cacheSolve
## @argument x matrix
## checks whether supplied matrix is identical to
## global matrix created by makeCacheMatrix function or not
## checks existence of global inverse matrix (invmatrix) 
## if inverse matrix exists then return that
## else compute the inverse of supplied matrix 
## finally update the global matrix and inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check identical and invmatrix exists
  if(identical(copyxmatrix,x)) {
    if(exists("invxmatrix")) {
      message("getting cached inverse matrix")
      return(invxmatrix)
    }
    else {
      message("computing inverse of supplied matrix")
      ## update globals
      copyxmatrix <<- x
      invxmatrix <<- solve(x)
      return(invxmatrix)      
    }
  }
  else {
    message("computing inverse of supplied matrix")
    ## update globals
    copyxmatrix <<- x
    invxmatrix <<- solve(x)
    return(invxmatrix)
  }
}
