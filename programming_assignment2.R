## this function prepares the cache with the inverse
## for 1st move, it is empty and gets value after calculation..

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks whether the cache has the inverse.
## if cache has the inverse then the cached inverse is displayed.
## else the inverse is calculated and saved in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    print("Cache memory retrieved")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  print(inv)
}
