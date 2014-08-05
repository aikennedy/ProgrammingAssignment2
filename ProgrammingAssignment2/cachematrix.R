## makeCacheMatrix, cacheSolve
##   Allow the inverse of a matrix to be cached, with the matrix stored as a list containing
##   functions (set, get, setInverse, getInverse), created using makeCacheMatrix. The inverse
##   can be accessed by calling cacheSolve on the resulting list.

## makeCacheMatrix
##   given a matrix x as its parameter, this function creates a list containing the functions
##   (set, get, setInverse, getInverse) which allow the matrix to be accessed and updated, and
##   the inverse stored when computed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve
##   for a "matrix" created using makeCacheMatrix, this function will return the inverse,
##   using the stored value if it has already been computed. The "matrix" to be inverted
##   should be given as the first parameter, any other parameters will be passed to solve()

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
