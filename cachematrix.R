## makeCacheMatrix create a special "matrix" whose inverse can be cached
## cacheSolve solves the the special "matrix" of makeCachedMatrix


## makeCacheMatrix create a special "matrix" which is really a list containing 4 functions
## to set the matrix, get the matrix, get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  getInverse <- function(){
    if(is.null(i)){
      i <<- solve(x)
      return(i)
    }else{
      return(i)
    }
  }
  list(set=set, get=get, getInverse=getInverse)
}


## cacheSolve returns the inverse of matrix if only one matrix is passed in.
## if not, cacheSolve will use solve() to get the correct answer

cacheSolve <- function(x, ...) {
  if(length(list(...)) == 0){
    return(x$getInverse())
  }else{
    data <- x$get()
    i <- solve(data, ...)
    return(i)
  }
}
