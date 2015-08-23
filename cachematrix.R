## These two functions are used to generate and store an inverse matrix in cache in order to
## reduce the time to retrieve the inverse matrix. If the inverse matrix is not already in cache,
## then the inverse matrix is generated.

# This function establishes a list of functions that are used to get and store the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## cacheSolve is a function that receives a matrix, and returns the inverse of that matrix
## the function checks system cache to determine if the inverse matrix has already been 
## generated and is in cache. if it is then the inverse matrix is returned from cache. if not,
## the inverse matrix is generated and returned

cacheSolve <- function(x, ...) {
 
 invMat <- x$getSolve()
  # check to see if the inverse matrix has already been calculated and is in cache,
  # if so, provide a message back to user that the system is retrieving the data from cache.
  # then return the inverse matrix
  
  if(!is.null(invMat)){
    message("getting cached data")
    return (invMat)
  }
  # if the inverse matrix is not in cache, provide a message that the inverse matrix is being
  # generated, then generate the inverse matrix and return it
  
  message ("generating inverse matrix")
  data <- x$get()
  invMat <-solve(data, ...)
  x$setSolve(invMat)
  invMat
}

