##First create makeCacheMatrix which creates a special matrix and caches its inverse
##It assumes the matrix given is invertible.
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){  ## This function sets a new matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ##This function returns the matrix 'x'
  setinverse <- function(inverse) inv <<- inverse ##Here we set the inverse of the matrix obtained in 
                                                  ##cacheSolve to save it within the special matrix object
  getinverse <- function() inv ##This function returns the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

  ##This function calculates the inverse of the special matrix created before
  ##If the object allready contains the inverse within this function prints it
cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  print(inv)
}
