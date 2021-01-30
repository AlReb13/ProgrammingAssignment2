## Basically, the functions used in this program will cache the inverse of
## provided matrices.

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {invrs <<- inverse}
  getInverse <- function() {invrs}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The functions below will be caching the inverse of a matrix rather than
## computing it, if the inverse is already available. This will help the
## system in becoming more efficient.

cacheSolve <- function(x, ...) {invrs <- x$getInverse()
if(!is.null(invrs)){
  message("getting cached data")
  return(invrs)
}
m <- x$get()
invrs <- solve(m,...)
x$setInverse(invrs)
invrs
        ## Return a matrix that is the inverse of 'x'
}
