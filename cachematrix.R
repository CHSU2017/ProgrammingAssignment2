# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#If the inverse has already been calculated, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Sample run:
## x <- matrix(c(1,2,3,4),2,2)
## m = makeCacheMatrix(x)
## m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(m)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
