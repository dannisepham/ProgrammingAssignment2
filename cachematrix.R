## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fuction receives a matrix x and makes a special matrix x
## The special matrix has 4 functions: set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversion) inv <<- inversion
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function receives a sepecial matrix as an input
## It firstly checks whether there is a matrix inversion in cache
## If so, it returns the value in cache
## Otherwise, it computes matrix inversion of x

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## Compute matrix inversion
  inv <- solve(data)
  x$setinv(inv)
  inv
}

## Example:
x <- makeCacheMatrix(rbind(c(1,-1/4),c(-1/4,1)))
cacheSolve(x)
