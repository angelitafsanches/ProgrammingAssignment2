## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
##Creates a special matrix and cache his inverse matrix
  m <- NULL
  set <- function(y) {
    ##Set the value of the vector
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ##Get the value of the vector 
  setsolve <- function(solve) m <<- solve ##set inverse matrix value on cache
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


cacheSolve <- function(x, ...) {        
  ##Checks if the inverse matrix value is cached and if it is not, 
  ##this function calculates it
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)##Calculating inverse matrix
  x$setsolve(m)
  m
}
