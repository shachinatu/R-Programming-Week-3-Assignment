## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix is a function that creates square matrix x
#from vector y 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse=NULL
  set <- function(y) {
    #set x to vector y and size r*c
    set <- function(y) 
      x <<- y
      inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) i<-inverse  #set i=inverse
  getinverse <- function() i     # get the value of inverse stored in i
  
  list(get = get, set = set, setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
