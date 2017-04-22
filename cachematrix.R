## makeCacheMatrix creates a special vector that is a
## list that allows you to 'set' the value of the matrix
## 'get' the value of the matrix, 'set' the value of the
## inverse of the matrix, and 'get' the value of the 
## inverse of the matrix. cacheSolve will either return
## the inverse of a matrix from the cache or calculate and
## store the inverse in the cache for later use.

## makeCacheMatrix is a list of functions that allow the user
## to store and retrieve the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Checks to see if the inverse of the matrix has been calculated.
## If it has, it will return the inverse matrix. If it has not it
## will calculate the inverse matrix and store it in the cache for
## later use via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                  message("getting cached matrix")
                  return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}