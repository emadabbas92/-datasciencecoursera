## This assigment uses two functions 
##'makeCacheMatrix' and 'cacheSolve' 
##to make a special object that stores an invertible matrix,
## calculate it's inverse and cache it. 





##  makeCacheMatrix makes a special "matrix",
##  which is really a list containing a function to 
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse
##  4.get the value of the inverse
## It uses the other function 'cacheSolve' 
## to calculate and then cache the inverse of the special matrix 
## made through 'makeCacheMatrix'.


makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the inverse of the special "matrix" made with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data (matrix)
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
