## Create 2 functions. Function 1 (makeCacheMatrix) caches the inverse of a matrix. 
## Function 2 (cacheSolve) computes the inverse of a matrix, unless the inverse
## has already been calculated, in which case it returns the string "getting cached data"

## makeCacheMatrix caches the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a matrix, pulls in the inverted matrix from makeCacheMatrix
## and if it's not empty returns the message "getting cached data". Otherwise it 
## inverts the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
