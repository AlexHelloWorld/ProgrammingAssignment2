## makeCacheMatrix create a special "matrix" whose inverse can be cached
## cacheSolve solves the inverse of the special "matrix" of makeCachedMatrix

## Write a short comment describing this function
## makeCacheMatrix create a special "matrix" which is really a list containing 4 functions
## to set the matrix, get the matrix, get the matrix inverse and set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(j){
    i <<- j
  }
  getInverse <- function(){
    i
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}




## cacheSolve return the inverse of matrix if there is a cached inverse solution. If there isn't
## cacheSolve will compute the inserve, save it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }else{
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    return(i)
  }
}
