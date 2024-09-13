## Put comments here that give an overall description of what your
## functions do
## This Function will return inverse of square matrix and will save the result in cache as a getinverse variable
## Function will return value from cache if the input had already been computed from previous computation
## Write a short comment describing this function
## makeCacheMatrix function will save the new input variable, input variable x, set its new inverse variable, and get its inverse variable in to list variable
## This is as cache procedure function
makeCacheMatrix <- function(x = matrix()) {
	n <- nrow(x$get())
	m <- matrix(,n,n)
	set <- function(y) {
		x <<- y
		m <<- matrix(,n,n)
  }
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
## This Function will check if the input variable inverse had been calculated in cache.
## If variable had already been computed then function will return inverse value from cache 
## Otherwise input variable will be considered as new variable and inverse matrix will be computed and store to setinverse variable and will be stored to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
		if(!all(is.na(m))) {
		message("getting cached data")
		return(m)
  }
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
