
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation so there may be some benefit to caching 
## the inverse of a matrix instead of computing it repeatedly.
## This assignment writes a pair of functions that store a matrix, compute the inverse and then cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv_matrix <<- inverse
  getInv <- function() inv_matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The function below computes the inverse of the special "matrix" created by the
## makeCacheMatrix function above. If the inverse has already been calculated it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getInv()
  
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setInv(inv_matrix)
  inv_matrix  ## Return a matrix that is the inverse of 'x'
}
