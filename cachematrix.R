
# To create a funtion that calculates the inverse of a matrix.

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  setinverse <- function(inverse)inv <<-inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
  
}


#The "cacheSolve" function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result from the cache and skips the
# computation. Else, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
