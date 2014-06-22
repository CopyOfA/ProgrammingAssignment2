## These functions serve to search for a cached version of the inverse of the
## matrix provided. If none is found, the matrix inverse is computed

## The first function makeCacheMatrix creates a list containing four functions 
## which set the value of the matrix, get the value of the matrix, set the value
## of the matrix inverse, and get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      invX <- NULL
      set <- function(Y){
            x <<- Y
            invX <<- NULL
      }
      get <- function() x
      setinv <- function(solve) invX <<- solve
      getinv <- function() invX
      list(get = get,
           setinv = setinv,
           getinv = getinv)  
}


## This function searches for the inverse of the provided matrix, and if it is not
## cached, then it is computed

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invX <- x$getinv()
      if(!is.null(invX)){
            message('retrieving cached data')
            return(invX)
      }
      data <- x$get()
      invX <- solve(data,...)
      x$setinv(invX)
      invX
}