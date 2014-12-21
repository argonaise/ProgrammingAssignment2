## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  # property variable
  inv_cache <- NULL
  # method for access property variable
  get <- function() x
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  # cache related methods
  setcache <- function(new_inv_matrix) inv_cache <<- new_inv_matrix
  getcache <- function() inv_cache
  # return special "matrix" object
  list(set=set, get=get, setcache=setcache, getcache=getcache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # look if special "matrix" have a cache
  m <- x$getcache()
  # if special "matrix" have a cache, use it
  if(!is.null(m)) {
    message("getting cached data")
    
    ## Return a matrix that is the inverse of 'x'
    return(m)
  }
  # no cache - calculate then store
  data <- x$get()
  m <- solve(data)
  x$setcache(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
