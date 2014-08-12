## R Programming - Assignment 2
## Author - Vishnu Chevli
## Here we are caching inverse of matrix 
## It will save extra processing if matrix 
## is unchanged in processing

## Following function take square matrix as
## argument and store its value along with 
## its inverse matrix value
makeCacheMatrix <- function(x = matrix()) {

  ## m is inverse of matrix value
  m <- NULL
  
  ## When value of matrix changes inverse
  ## of matrix will also changes
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Set and get function for inverse
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  ## Returning get and set method for 
  ## matrix and its inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This fucntion find out inverse of input 
## Matrix, if inverse is not already calculated

cacheSolve <- function(x, ...) {
  ## Getting inverse value
  m <- x$getsolve()
  
  ## If inverse is found
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If inverse not found then this part
  ## calculate that value
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
