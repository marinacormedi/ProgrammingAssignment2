## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  

  i <- NULL
  

  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  

  get <- function() {
   
    m  ## Return the matrix
  }
  

  setInverse <- function(inverse) {
    i <<- inverse   ## set the inverse of matrix
  }
  
  
  getInverse <- function() {
    ## get the inverse 
    i
  }
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. 
cacheSolve <- function(x, ...) {
  
  
  m <- x$getInverse() ## Return the inverse of 'x'
  
 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  
  m <- solve(data) %*% data
  
 
  x$setInverse(m)
  
  ## Return the matrix
  m
}