# This function creates a special "matrix" object that can cache its inverse.
# See detailed comments inside the function

makeCacheMatrix <- function(x = matrix()) {
  #Step 1: setmatrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Step 2: get matrix
  get <- function() x
  #Step 3: set inverse of the matrix
  setInverseM <- function(inverse) inv <<- inverse
  #Step 4: get inverse of the matrix
  getInverseM <- function() inv
  #Step 5: set list
  list(set=set, get=get, setInverseM=setInverseM, getInverseM=getInverseM)
}


# This function below checks if inverse exists and then returns it. If it doesn't exist, it computes it.
# See detailed comments inside the function

cacheSolve <- function(x, ...) {
  inv <- x$getInverseM()
  #Step 1: check if the inverse already exists
  if(!is.null(inv)) {
    #Step 2: inverse exists - return it.
    return(inv)
  }
  # Step 3: inverse doesn't exist yet - compute it and return it
  data <- x$get()
  inv <- solve(data)
  x$setInverseM(inv)
  inv
}