## makeCacheMatrix and cacheSolve paried functions
### These two function were used together to save the matrix and inverse Matrix into cache, 
### so that you donot need to calculate the inverse of a matrix repeatly if you need to use it many times. 
###
### Normally, makeCacheMatrix were used first to creat a special matrix that can cache its invers matrix.
### 
### then when you need to pass the special matrix to cacheSolve function, it will return the inverse matrix with a message "getting cached data" if the inverse matrix has already been saved 
### in the cache, otherwise it will calcalte the inverse matrix and update the inverseve matrix into cache.


## makeCacheMatrix function 
###creates a special "matrix" object that can cache the matrix and its inverse, which is really a list containing a function to
####1.set the value of the matrix
####2.get the value of the matrix
####3.set the value of the inverse matrix
####4.get the value of the inverse matrix
### 1 input arguement: "x" is which matrix you want to get its inverse matrix. 


makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) Inv<<- solve
  getInv <- function() Inv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve function: 
### 1 input argument:  
####    "x" is the specail matrix created by makeCacheMatrix function before.
###  if the inverse valued saved in cache is not NULL, then this function will retrun the saved inverse matrix.
###  otherwise, the inverse matrix will be calculated according to x, and the new inverse matrix will be updated in cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  
  data<-x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
  
}
