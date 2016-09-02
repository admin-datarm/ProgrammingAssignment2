## The functions are aimed to compute the inverse matrix with a cache if it's computed previously.

## This function provide list of functions to be able to cache the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y){
		x <<- y
		mat <<- NULL
	}
	get <- function() x
	setInverseMatrix <- function(inverseMat){
		mat <<- inverseMat
	}
	getInverseMatrix <- function() mat
	list(set=set,get=get,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)
}


## This function solve the inverse matrix given a square invertible input matrix, if the inverse matrix have been computed, return it from cache directly.

cacheSolve <- function(x, ...) {
	inverseMat <- x$getInverseMatrix()
	if(!is.null(inverseMat)){
		message("getting cached data")
		return(inverseMat)
	}
	data <- x$get()
	inverseMat <- solve(data,...)
	x$setInverseMatrix(inverseMat)
	inverseMat
}
