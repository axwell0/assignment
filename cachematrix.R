## This function aims to create the matrix we're going to be caching
## It first starts by defining the functions of cached matrix creation such as 
## getting the matrix, setting its new value, fetching and setting the inverse
## returns a list of those functions to allow cacheSolve to do the work

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  list(set=set,get=get,getinv=getinv,setinv=setinv)
  
}


## gets the inverse from the list passed on by makeCacheMatrix and returns it if it exists
## else it calculates the inverse and returns it.
cacheSolve <- function(x,...)
{
  inv <- x$getinv()
  if (!is.null(inv))
  {
    message("getting cached inverse matrix")
    return(inv)
  }
  current <- x$get()
  inverse <- solve(current,...)
  x$setinv(inverse)
  inverse
  
}