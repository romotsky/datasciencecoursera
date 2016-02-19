
## makeCacheMatrix creates a special matrix object that can cache its inverse

## comments are inline with each command

makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL  ## gives cache initial value of NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  } ## this sets the value of the matrix
  get <- function() x ## this gets the value of the matrix
  setinv <- function(solve) cache <<- solve  ## this sets the value of the matrix inverse
  getinv <- function() cache ## this gets the value of the matrix inverse
  list( set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
      ) ## this prints out the formulas
}


## cachSolve computes the inverse of the special matrix from the makeCacheMatrix formula. 
## If the inverse has already been calculated and the matrix has not changed, then this 
## formula retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getinv()  ## loads previous value of matrix inverse
  if(!is.null(cache)) {  
    message("getting cached data")
    return(cache)
  } ## if previous value of matrix inverse exists, then print message and return previous value 
  else { ## if not...
    data <- x$get()  ## assigns value of matrix
    cache <- solve(data, ...) ## run the inverse formula
    x$setinv(cache) ## sets inverse to setinv
    cache ## prints newly calculated matrix
  }
}


## then I can assign a the makeCacheMatrix formula to a variable
## samp_matrix <- matrix(c(43543,645645,23432,234432),2,2)
## samp_matrix ## prints out matrix
## test_make <- makeCacheMatrix(samp_matrix) ## loads matrix to the set formula
## cacheSolve(test_make)  ## rretrieves the cached results and prints

