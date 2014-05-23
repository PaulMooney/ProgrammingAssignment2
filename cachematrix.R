## Overall we aim to allow the code to accept a square matrix
## and return the invert of it. We will also cache this
## result the first time so that when the invert is called
## subsequent times the result will be returned from the cache
## to avoid having to re-run the inverting logic again.




## The makeCacheMatrix function will accept a matrix which we
## will assume is invertible. The function will return a list
## of four functions which internally support the get and set
## methods for the invert method. The return value (i.e. the 
## inverted matrix) will be stored in the i variable using the
## <<- operator to ensure it retains it's value after it is run.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## The cacheSolve function will be used to invoke the
## setinvert method and the getinvert method from the 
## the list object returned from the makeCacheMatrix function.
## The return result is stored in the variabe i. Note that if
## i is NULL the setinvert is called to fill it the first time
## and return it's value. In subsequent calls, because i will
## not be NULL the message "getting cached data" will be printed
## before the value is returned to verify it came from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i    
}

#This is just code to verify my functions work as intended
my.matrix <- matrix(c(1,0,3,2,2,4,3,2,1), ncol=3)

cache.matrix <- makeCacheMatrix(my.matrix)

#First time around the inert will not be cached but calculted
cacheSolve(cache.matrix)

#Second time around we will get the cached version
cacheSolve(cache.matrix)

