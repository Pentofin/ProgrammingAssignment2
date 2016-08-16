## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	 minv <- NULL #matrix is Null
  set <- function(y) {
    x <<- y #sets variable as user-defined matrix
    minv <<- NULL
  }
  get <- function() x #retrieve matrix
  setInverse <- function(minverse) minv <<- minverse
  getInverse <- function() minv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getInverse()
  if (!is.null(minv)) {
    message("getting cached data")
    return(minv) #returns NA matrix with above message
  }
  mat <- x$get()
  minv <- solve(mat, ...) #calculates matrix inverse if matrix is not Null
  x$setInverse(minv)
  minv
}
