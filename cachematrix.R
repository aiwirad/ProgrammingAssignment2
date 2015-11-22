## Put comments here that give an overall description of what your
## functions do

## This function is create matrix for Program Assignment 2
##

makeCacheMatrix <- function(m = matrix(), x=numeric()) {
  rslt <- NULL
  crtmtrx <- function(b) {
    m <<- b
    rslt <<- NULL	
  }
  getmtrx <- function() matrix(m,nrow=x,ncol=x)
  crtinv <- function(solve) rslt <<- solve
  getinv <- function() rslt
  list(crtmtrx=crtmtrx, getmtrx= getmtrx, crtinv=crtinv, getinv=getinv)
}


## The following function will check if the inverse matrix is available in cache or not.
## If not, it will add the inverse matrix based on x

cacheSolve <- function(x, ...) {
  rslt <- x$getinv()
  if(!is.null(rslt)){
    message("getting carslted data")
    return(rslt)
  }
  tm <- x$getmtrx()
  rslt <- solve(tm, ...)
  x$crtinv(rslt)
  rslt
}
