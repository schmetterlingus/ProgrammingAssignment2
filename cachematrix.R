## Inverting matrices
## Cash data 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x 
  
  setinv <- function(solve) m <<- solve
  
  getinv <- function() m
  
  list( set=set, get=get, setinv=setinv, getinv=getinv )
}

## Invert matrices using cashed data
cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinv()  
  if( !is.null(m) ){ #check if the matrix is cached
    message( "getting cached data" )
    return( m )
  } 
  data <- x$get()
  m <- solve( data, ... )
  x$setinv( m )
  m
}
