## These functions create cache matrix and solve matrix operations in R
## The first function defines the list of functions and makes space for inputs
## The second function checks for an identical value in the cache
## and solves the matrix then saves the new matrix if not in the cache

makeCacheMatrix <- function(x = matrix()) {

  ## creates the function objects and parent objects (the cache)
  m <- NULL
  set <- function(y) {
    x <<- y     #using Lexical Scoping 'outside' of working environment
    m <<- NULL
  }  
  #retrieves the matrix value and stores new value in the cache
  get <- function() x
  setcache <- function(solve) m <<- solve  #puts this value into cache called 'm'
  getcache <- function() m                 #retrieves 'm' from cache
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
  
}

## Processes the matrix operations

cacheSolve <- function(x, ...) {
        
    m <- x$getcache()
    
  # check to see if the cache is holding a matrix  
  if(!is.null(m)) {
    message("Fetching cached data if available")
    return(m)
  }
  
  ## Invert 'x' and solve the matrix then set the cache with new value
  
  data <- x$get()
  m <- solve(data, ...)   # the solve inverts the data
  x$setcache(m)           # this sets the answer 'm' to the cache above
  m

}


