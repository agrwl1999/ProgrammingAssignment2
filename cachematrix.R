## Write a short comment describing this function

## ------------------------------------------------------------------------

## Retrieving the matrix inverse by the following R code
## A set of two functions- "makeCacheMatrix" & "cacheSolve" are defined
## First function "makeCacheMatrix" creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL 
	## initialising matrix inverse as null
	set<-function(y){
	## assigning value of function y in set
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
	## a set of commands which will compute the inverse of a matrix
}
 	
## Second function "cacheSolve" calculates the inverse of that special matrix
## created by first function defined above and then checking the condition 
## that if inverse has already taken than in that case matrix inverse will be
## retireved from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	inv<- x$getInverse()
	## evaluating and getting matrix inverse 
      if(!is.null(inv)){
		message("matrix retrieved")
	## if already computed then print that matrix already retrieved
		return(inv)
	}
	mat<-x$get()
	inv<-solve(mat,...)
	## using solve function finally to get the inverse from cache
	x$setInverse(inv)
	inv
}
