## the two functions below are used to cache the inverse of a matrix

## this function creates a special matrix that can cache its inverse
## set value of matrix
## get value of matrix
## set value of inverse of matrix
## get value of inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y){
    x <<- y
    cache <<- NULL
  } 
  get <- function() x
  setmatrix <- function(inverse) cache <<- inverse
  getinverse <-function() cache
  list(set = set, get = get, setmatrix = setmatrix, getinverse = getinverse)

}

## this function computes the inverse of the new matrix created in makeCacheMatrix
## if the inverse has already been caluclated (and the matrix is the same), then the function gets the result and skips the computation
## if the inverse has not been calculated, the function sets the value in the cache with the setmatrix function
## the solve function computes the inverse of a square matrix

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting the cached data")
    return(cache)
  }
  matrix <- x$get()
  cache <- solve(matrix)
  x$setmatrix(cache)
  cache
}
