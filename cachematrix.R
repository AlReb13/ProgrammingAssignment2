#Basically, the functions used in this program will cache the inverse of 
#provided matrices.

#Creating the special main function (a vector) which contains other pertinent 
#functions:
makeCacheMatrix <- function (x=matrix()){ 
  i <- NULL
  #Defining a function that will contain or set the value of the matrix;
  #The use of the <<- operator suggests that the assigned value to the variable 
  #is a different environment, i.e. in the parent level:
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #Defining a function that will get the value of the matrix:
  get <- function() {x}
  #Setting & getting the value of the inverse of the matrix:
  setInverse <- function(inverse) {i <<- inverse}
  getInverse <- function() {i}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


#The functions below will be caching the inverse of a matrix rather than
#computing it, if the inverse is already available. This will help the system in 
#becoming more efficient.

cacheSolve <- function(x,...){
  #This code returns the inverse matrix of a variable:
  i <- x$getInverse()
  #Checking if the inverse of the matrix has already been computed; if so,
  #computation of its inverse will be skipped:
  if(!is.null(i)){
    #This message will be displayed if the inverse is already present in the 
    #previous cache:
    message("getting cached data")
    return(i)
  }
  #This part will compute for the inverse of the matrix if it is still not 
  #provided:
  m <- x$get()
  i <- solve(m,...)
  x$setInverse(i)
  i
}