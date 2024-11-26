#This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getInverse <- function() inv  # Get the cached inverse
  #these are the keys which will be used in cacheSolve function below
  list(set = set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The next function computes the inverse of the special matrix returned by the function above.
## if the inverse has already been calculated it retrieves the inverse from the cache. 
##Otherwise, it computes the inverse, stores it in the cache, and returns it

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the cached inverse
  # If cached, return it
  if (!is.null(inv)){ 
    message("getting cache data")
    return(inv)
  }
  data <- x$get()   #retreive the original matrix
  inv <- solve(data, ...) # compute the inverse of the matrix
  x$setInverse(inv)  #cache the newly computed inverse
  inv  #return the inverse
}
