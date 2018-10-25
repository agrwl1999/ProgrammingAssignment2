## Write a short comment describing this function

## ------------------------------------------------------------------------

## Retrieving the matrix inverse by the following R code
## A set of two functions- "makeCacheMatrix" & "cacheSolve" are defined
## First function "makeCacheMatrix" creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y	
		inv<<-NULL
	}
	get<-function()x
	setInverse<-function(inverse) inv<<-inverse
	getInverse<-function()inv
	list(set=set,
	     get=get,
           setInverse=setInverse,
           getInverse=getInverse)
}
 	
## Second function "cacheSolve" calculates the inverse of that special matrix
## created by first function defined above and then checking the condition 
## that if inverse has already taken than in that case matrix inverse will be
## retireved from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	inv<- x$getInverse()
      if(!is.null(inv)){
		message("matrix retrieved")
		return(inv)
	}
	mat<-x$get()
	inv<-solve(mat,...)
	x$setInverse(inv)
	inv
}
