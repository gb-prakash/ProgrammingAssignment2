## Function cacheSolve() computes the inverse of the matrix created by the makeCacheMatrix() function.
# If the Inverse of the matrix is already calculated,It retrieves the inverse from the cache, 
# else, It fetches the matrix created by the makeCacheMatrix() function and calculates its inverse.
makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse.
## It consists of four parts,
## 1.set: It creates the matrix. 
## 2.get: It fetches the matrix created.
## 3.setinverse: It creates the inverse of the matrix fetched.
## 4.getinverse: It fetches the inverse calculated.
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x' from the cache,
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
## else,It calculates the inverse and returns it.
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
