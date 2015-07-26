## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #Assign m as a null object
  m <- NULL
  
  #Create function set that stores the value y
  # in to global var x and a null value for global var m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Create a function to return the value of local X
  get <- function() x
  
  #Set the value passed as Invert into the global m
  setInv <- function(Invert) m <<- Invert
  
  #Return the value in m
  getInv <- function() m
  
  #Return a list of functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##   the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Setzz a matrix that is the inverse of 'x'
  m <- x$getInv()
  
  #If m is not null, return the precalculated data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Get a subset of the data from x
  data <- x$get()
  
  #Run the inverse function solve
  m <- solve(data, ...)
  
  #Return the subset
  x$setInv(m)
  m  
}
