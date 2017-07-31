## These two fuction will create a matrix that has a cache for it's inverse and be able to inverse the matrix

## This function creates a matrix that can have a cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
    set <- function(y = matrix()) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## This function checks to see if the cache has the inverse already and if it does not then it inverses
## the matrix and saves it to the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
