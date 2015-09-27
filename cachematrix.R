## Similar with the Example of caching the mean of the vector. First we need to create a 
##function to create a special "matrix" to cache its inverse. 

#Then the second function to compute its inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

##Check the functions

mat<-makeCacheMatrix(matrix(c(2,5,3,3,2,4,7,6,4,3,8,0,7,5,3,2),4,4))
mat
mat$get()
mat$getInverse()
cacheSolve(mat)

cacheSolve(mat)

