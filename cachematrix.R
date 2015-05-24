## Put comments here that give an overall description of what your
## functions do 

#cachematrix.R: A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  list( set=set, 
        get=get, 
        setInverse=setinverse,
        getInverse=getinverse )
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
