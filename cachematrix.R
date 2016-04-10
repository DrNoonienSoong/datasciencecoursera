## This function is nearly identical to the makeVector instead it does the follwoing:
## 1) Set the value of a matrix
## 2) Get the value of a matrix
## 3) Set the value of the inverted matrix
## 4) get the value of the inverted matrix

makeCacheMatrix<- function(x = matrix()){
  inverted<-NULL
  set<- function(y){
    x<<- y
    inverted<<- NULL
  }
  get<-function()x
  setinverse <- function(inverse)inverted <<- inverse
  getinverse <- function()inverted
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function calculates the inverse of the matrix from the avbove function
## after first checking to see if the mean has already been calculated and chaced
## If the inverted matrix is cached then it gets it and skips the computation.
## otherwise the function calculates the inverse of the matrix. 
cacheSolve <- function(x, ...){
  inverted<- x$getinverse()
  if(!is.null(inverted)){
    message("getting cached data")
    return(inverted)
  }
  data<- x$get()
  inverted<- solve(data, ...)
  x$setinverse(inverted)
  inverted
}