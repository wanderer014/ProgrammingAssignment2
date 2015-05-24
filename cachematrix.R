## Put comments here that give an overall description of what your
## functions do 

#cachematrix.R: A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Define getters and setters:
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  #Return a list of functions
  list( set=set, 
        get=get, 
        setInverse=setinverse,
        getInverse=getinverse )
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #Try to get Inverse from cache:
  inv <- x$getInverse()
  if(!is.null(inv)) {
    #Inverse found in Cache!
    message("getting cached data")
    return(inv)
  }
  #Inverse not found in Cache
  data <- x$get()
  inv <- solve(data)
  #Store calculated inverse in Cache
  x$setInverse(inv)
  inv
}
