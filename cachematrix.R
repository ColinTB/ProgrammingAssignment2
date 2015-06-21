## The two functions cache the inverse of a matrix
## makeCacheMatrix generates a list with a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinver <- function(inver) inv <<- inver
  getinver <- function() inv
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}



## CacheSolve gives the inverse. First it checks if the 
## inverse already exists, if so it provides the inverse
## if no then it calculates the inverse using setinver

## *assumes the inverse exists

cacheSolve <- function(x, ...) {
  inv <- x$getinver()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinver(inv)
  inv
}

#if you want to test it, create any matrix x
## run m = makeCacheMatrix(x)
## now use m$get() to get the matrix
## cacheSolve(m) to solve it
## run this last line again, and you should be prompted
## that you are getting cached data, since the inverse
## has already been solved for
## TADA!

