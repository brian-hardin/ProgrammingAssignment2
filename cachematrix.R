## makeCacheMatrix - this function creates a special "matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
      list(
        set = set,
        get = get,
      setinverse = setinverse,
      getinverse = getinverse
  )
}


## cacheSolve - This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
      m <- x$get()
      i <- solve(m, ...)
      x$setinverse(i)
      i
}
