## The 'makeCacheMatrix' function takes an input matrix and creates an object that
## stores the matrix and its inverse, initializing the inverse to NULL whenever
## new data is provided. Internally, this is done through the 'set' function. The 
## other functions used within 'makeCacheMatrix are: 
## 'get':    gets the input matrix
## 'setinv': uses the R function 'solve' to compute the inverse of the input matrix
## 'getinv': returns the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The 'cacheSolve' function takes an object x, containing a matrix and its 
## inverse, created by 'makeCacheMatrix.
## The function begins by trying to get the inverse from x. 
##    * If the inverse is not NULL, the function returns the inverse
##    * If the inverse is NULL, this function gets the matrix from x, computes
##      its inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

