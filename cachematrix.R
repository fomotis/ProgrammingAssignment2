## This function creates a special "matrix" object that can cache its inverse
## inform of a list

## Function that caches a matrix

makeCacheMatrix <- function(x = matrix()) 
{
  sinv <- NULL
  set <- function(mee)
  {
    x <<- mee
    sinv <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) sinv <<- solve
  getsolve <- function() sinv
  list(set = set ,get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function checks if inverse of matrix created 
## by the makeCacheMatrix function has been calculated
## and returns its value or calculates it if it has 
## not been calculated

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  sinv <- x$getsolve()
  if(!is.null(sinv))
  {
    message("Getting Cached Inverse")
    return(sinv)
  }
  data <- x$get()
  sinv <- solve(data)
  x$setsolve(sinv)
  sinv
  
}
