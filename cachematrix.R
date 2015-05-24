## makeCacheMatrix creates different auxilary functions that are applied in cacheSolve to invert a matrix
## either by bruto force or retrieving the inverted matrix from the computer's cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set fuctions write x=y and m=NULL to the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get returns the original matrix
  get <- function() x
  
  #set inv funtion writes the inverse of a matrix into the cache
  setinv <- function(solve) m <<- solve
  
  #getinv returns the cached inverted matrix
  getinv <- function() m
  
  #return a list of results from the auxilary functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  cacheSolve inverts the matrix if not found in cache
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() 
  if(!is.null(m)) {  
    
    #if m is not null return data from cache
    message("Getting data from cache")
    
    #return m which is the inverted matrix from cache (breaks the funtion run)
    return(m) 
  }
  matrix <- x$get()
  
  #else (if m = NULL) use function solve to invert the  matrix
  m <- solve(matrix, ...) 
  
  #store intverted matrix into the cache
  x$setinv(m) 
  
  #return inverted matrix to the user
  m
}
