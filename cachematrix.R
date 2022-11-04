## Put comments here that give an overall description of what your
## functions do

## This will create a function that creates a matrix object that caches its 
## inverse
## Using the example of 'Caching a Mean of a Vector' I simply substituted the 
## 'mean' functions with 'solve'
## 'iv' is the input  

makeCacheMatrix <- function(x = matrix()) ##setting matrix to default 
{
  iv <- NULL
  set <- function(y) {
    x <<- y  ##sets value in parent environment
    iv <<- NULL ##setting iv to NULL if new matrix
  }
  get <- function() x
  setsolve <- function(solve) iv <<- solve 
  ##replaced the 'means' with 'solved'
  getsolve <- function() iv ##replace 'get/setmean' to 'get/setsolve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



##This function computes the inverse of the matrix returned by 'makeCacheMatrix'
## From example, converted all 'getmean/setmean' to 'getsolve/setsolve'

cacheSolve <- function(x, ...) {
  iv <- x$getsolve()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setsolve(iv)
  iv
}

## Tested using the 'simple test matrix for lexical scoping program assignment'
## forum post by Alan E. Berger, results matched.