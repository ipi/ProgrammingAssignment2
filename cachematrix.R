## These functions are used to cache matrix inversion to make 
## looping of big matrix operations faster.

## I'm going to use commenting style learned
## from the book "Learn Python the Hardway"
## and write comments above each line of code.
## It's not the pretiest style, but it's gonna
## make the code much easier to understand for
## myself and evaluators.

## First function makeCacheMatrix takes a inversible square matrix 
## as it's input and give's out list of 4 functions. Other functions
## can use this list to cache matrix inversion.
## For example "a <- makeCacheMatrix(matrix(rnorm(100), 10, 10))" 
## creates a list of functions for this 10x10 matrix with random values.  

makeCacheMatrix <- function(x = matrix()) {
  ## Create a variable i, which stores the matrix inverse
  ## & assign it's value to NULL
  i <- NULL
  ## This is first item function in the list and it's used
  ## to cache a new matrix
  set <- function(y) {
    ## cache new matrix y and assign it to x
    x <<- y
    ## set value of cached matrix inverse back to NULL
    i <<- NULL
  }
  ## Second list item "get" is a function used to get the 
  ## cached matrix
  get <- function() x
  ## third list item is a function which caches the matrix
  ## inverse of matrix x
  setInverse <- function(inverse) m <<- inverse
  ## fourth list item is a function to get the
  ## cached inverse of matrix x
  getInverse <- function() i
  ## next we store defined functions to a list which is
  ## actual output of this makeCacheMatrix function
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The second function takes a list of functions created 
## with makeCacheMatrix as it's input and print's out inverse 
## of the matrix.
## If we follow example given in comments above makeCacheMatrix,
## we can write "cacheSolve(a)" to get inverse of this 10x10 matrix 
## assigned to mtx.
## This function is useful in loops where inverse of big matrixes 
## would otherwise be calculated multiple times.

cacheSolve <- function(x, ...) {
  ## get the value of inverse stored in the cache
  ## and assign it to i
  i <- x$getInverse()
  ## if there already is a cached inverse value do something
  if(!is.null(i)) {
    ## tell user to get inverse from the cache
    message("getting cached matrix inverse")
    ## return value of cached matrix and exit function
    return(i)
  }
  ## If there is no cached inverse, start calculating.
  ## First get the cached matrix and assign it's
  ## value to data-variable
  data <- x$get()
  ## tell user to calculate inverse
  message("calculating matrix inversion")
  ## calculate the inverse of matrix assigned to 
  ## data and assign it to i. ...
  i <- solve(data, ...)
  ## cache calculated inverse using getCacheMatrix's 
  ## third listed function setInverse
  x$setInverse(i)
  ## print out matrix inverse 
  i
}