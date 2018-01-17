## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {   # initialization of x as a matrix variable and as a passing argument

  m <- NULL                                   # initialization of m variable that will be used as the matrix inverse value
  
  set <- function(y) {                        # function to set new matrix value in parent scope
    x <<- y                                 
    m <<- NULL
  }
  
  get <- function() x                         # function to get the matrix value
  
  setinverse <- function(solve) m <<- solve   # function to set the inverse matrix value
  
  getinverse <- function() m                  # function to get the inverse matrix value
  
  list(                                       # assigns all of the functions as an element within a list(), and returns it to the parent environment
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )  
}



## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()                   # retrieve an inverse from the object passed in as the argument
  
  if(!is.null(m)) {                     # checks to see whether the result is NULL
    message("getting cached data")
    return(m)                           # if the value here is not equal to NULL, we have a valid, cached inverse and can return it to the parent environment
  }

  data <- x$get()                       # If the result of !is.null(m) is FALSE, cachesolve() gets the vector from the input object,
  
  m <- solve(data, ...)                 # calculates an inverse by solve(),
  
  x$setinverse(m)                       # uses the setinverse() function on the input object to set the inverse in the input object, 
  
  m                                     # and then returns the value of the inverse to the parent environment by printing the inverse object
}