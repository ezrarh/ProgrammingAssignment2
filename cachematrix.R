## Takes in x which is matrix and returns a list of the following functions:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse
## 4. Get the inverse
## List is used as input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mtx <<- inverse
  getinverse <- function() mtx
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Takes output of makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## returns inverse of matrix input from makeCacheMatrix
  mtx <- x$getinverse()
  
  ## asks if inverse has been calculated and gets it
  if(!is.null(mtx)) {
      message("getting cached data")
      return(mtx)
  }
  
  # calculates inverse
  data <- x$get()
  mtx <- solve(data, ...)
  
  # sets value of the inverse in cache
  x$setinverse(mtx)
  
  return(mtx)
  
}
