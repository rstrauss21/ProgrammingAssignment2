## The following 2 functions will take a matrix as input and then
## will allow the user to retrieve the matrix, set a new matrix
## determine the inverse of the matrix and retrieve the matrix
## These functions will also place the inverse in cache and retrieve
## the cached version if it exists, so that the inverse matrix will
## not have to be computed each time

## This first function will define the functions to set the the matrix,
## retrieve the matrix, set the inverse of the matrix and retrieve
## the inverse of the matrix

## in order for the function to work properly, the variable that is set
## when calling the makeCacheMatrix needs to be called mcm

makeCacheMatrix <- function(x = matrix()) {
  ## This function will create a square matrix and   
  
  ## initializing the matrix to null
  x <- NULL
  
  ## defining the function "set" which will set the original matix
  setmatrx <- function(y) {
    x <<- y
    ix <<- NULL
  }
  ## defining the function "ret", which will retrieve the original matrix
  retmatrx <- function() return(x)
  
  ## defining the function "set" which will set the inverse matrix
  setinv <- function(inverse) ix <<- inverse
  
  ## defining the function "retinv" which will retrieve the inverse matrix 
  retinv <- function() return(ix)
  
  ## setting the return list
  list(setmatrx = setmatrx, 
       retmatrx = retmatrx,
       setinv = setinv,
       retinv = retinv)
}


## This function will take a matrix and return the inverse matrix
## If the inverse matrix already exists, it will return the stored inverse
## If the inverse matrix does not already exist, this function will
## determine the inverse matrix and return that one

cacheSolve <- function(x=matrix(), ...) {
  ## First let's see if the inverse exists
  ## If the $retinv() function returns an inverse matrix
  ## which is returned to the original calling function
  m <- mcm$retinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## At this point, we know that there is no stored  inverse matrix
  ## So let's compute the inverse and return that one
  data <- mcm$retmatrx()
  m <- solve(data)
  mcm$setinv(m)
  m
}