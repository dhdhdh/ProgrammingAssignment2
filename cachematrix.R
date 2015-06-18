## These functions calculates the inverse of an invertible square matrix and
##caches the result as long as the matrix did not change

## This function allows the input square invertible matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv<-NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  SetInv<-function(I) Inv<<-I
  GetInv<-function() Inv
  list(set=set, get=get, getinv=GetInv, setinv=SetInv)
}


## This function returns the cached inverse matrix of the input (calculated by 
## makeCacheMatrix function) as long as it is
## not changed. But it calculates the inverse when the input changes or 
##when it is not calculated before.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I<-x$getinv()
  if(!is.null(I))
  {
    message("getting cached data")
    return(I)
  }
  data<-x$get()
  I<-solve(data)
  x$setinv(I)
  I  
}
