## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$set_inverse(m_inv)
  m_inv
}
