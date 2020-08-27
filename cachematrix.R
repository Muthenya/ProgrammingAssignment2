

## The function below creates a matrix whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
          x <<- y
          m_inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m_inv <<- solve
  get_inverse <- function() m_inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


##This function checks to see if the matrix's inverse has already been computed and returns the vale
##Otherwise it computes the value

cacheSolve <- function(x, ...) {
        ## Checking if the inverse has already been computed
  m_inv <- x$get_inverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  ## Computing the inverse
  m_inv <- solve(data, ...)
  x$set_inverse(m_inv)
  m_inv
}
