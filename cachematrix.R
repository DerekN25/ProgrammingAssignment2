## cachematrix.R
##
## This function is meant to create a cache marix object
## which can in turn repeatably solve the inverse of the marix,
## but it will only calculate the inverse once.
##
## Usage:
##  a<-makeCacheMatrix()
##  a$set(matrix(1:4,2,2))
##  cacheSolve(a)
##


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This will return the inverse of cacheMatrix


cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
