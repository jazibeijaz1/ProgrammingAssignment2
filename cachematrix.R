
## Generates a special matrix with getters and setters for the
## original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function() {
    inverse
  }
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Computes the inverse of the special matrix and caches it for future reference
## as long as the value of the matrix is not updated

cacheSolve <- function(x) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat)
  x$setInverse(inverse)
  inverse
}
