##This function will set and get the value of matrix and its inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function will get the outcome of the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

x<-cbind(c(1,0,1),c(1,1,0),c(0,1,0))
m<-makeCacheMatrix(x)
m$get()
cacheSolve(m)
