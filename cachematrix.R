## This pair of funtions allow to cache the inverse of a matrix rather than compute it repeatedly.


##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  
  ## set() is used to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get() is used to fetch the matrix
  get <- function() x
  
  ## setinverse() is used to set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  
  ## getinverse() is used to fetch the inverse of the matrix
  getinverse <- function() m
  
  ## the following line stores the four funtions:
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## the following line verifies is the cached inverse is not NULL and returns the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else if cached inverse is NULL, the following lines calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}