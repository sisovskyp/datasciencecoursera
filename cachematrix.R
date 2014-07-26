##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##This function computes the inverse of the special "matrix" and retrieve already calculated inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()  ##I used amatrix$getsolve instead of amatrix$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)    ##ak chceme ulozit novy matrix
  m
}