## The below function creates a special Matrix



makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInver <- function(inverse) inver <<- inverse
  getInver <- function() inver
  list(set = set,
       get = get,
       setInver = setInver,
       getInver = getInver)
}


## Gets the inverse of the matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inver <- x$getInver()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInver(inver)
  inver
}
