## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## matrix inversion is costly computation  function.so for avoiding repeted computation we cache the 
## inversion matrix.we fallowing functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## Write a short comment describing this function
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
 

}


## sample testing
## x = rbind(c(4, -1/6), c(-1/3, 4))
## test = makeCacheMatrix(x)
## cacheSolve(test)
## cacheSolve(test)
## getting cached data
## [,1]       [,2]
## [1,] 0.25087108 0.01045296
## [2,] 0.02090592 0.25087108
