## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix with the cached inverse of the source matrix
## All the variables are preceded by the "v" letter

makeCacheMatrix <- function(vmatsource = matrix()) {
  ## @vmatsource: a square invertible matrix object
  ## return: a list containing functions to
  ##              1. get the matrix
  ##              2. set the inverse
  ##              3. get the inverse
  ##         this list is used as the input to the other function cacheSolve()
  
  ## Initialize the inverse matrix
  vmatinv <- NULL
  
  ## We define the functions to work with the matrix
  
  ## We get the original matrix
  get <- function() vmatsource
  ## We set the value of the inverse to the cache
  setinv <- function(inverse) vmatinv <<- inverse
  ## we get the cached inverse
  getinv <- function() vmatinv
  ## we return the functions to work with the cached matrix
  list(get=get, setinv=setinv, getinv=getinv)
}



## Write a short comment describing this function
## This function returns the inverse of a matrix. If the inverse has already been
## computed, we use the cache
## All the variables are preceded by the "v" letter

cacheSolve <- function(vmatsource, ...) {
        ## Return a matrix that is the inverse of 'vmatsource', either from a cache or making the calculation
  
  ## @vmatsource: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  vmatinv <- vmatsource$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(vmatinv)){
    ## get it from the cache and skips the computation. 
    message("getting the cached matrix")
    ## we return the cached matrix
    return(vmatinv)
    
  } else {
  
      ## otherwise, calculates the inverse 
      vmat.data <- vmatsource$get()
      vmatinv <- solve(vmat.data, ...)
  }

  # sets the value of the inverse in the cache via the setinv function.
  vmatsource$setinv(vmatinv)
  
  return(vmatinv)
}

