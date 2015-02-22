## The following functions are used to complete requores for ProgrammingAssignment2
## to create a cachedMatrix that caches the inverse of the matrix
##
## sample usage
## m <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(m)

## Generate a CacheMatrix.
## The generated or the parameter cache is required to be a square
## matrix
makeCacheMatrix <- function(x = matrix()) {
  d <- dim(x)
  if(d[1] != d[2]){
    message("invalid parameter. Square matrix required")
    stop()
  }
   s <- NULL
   set <- function(y) {
     x <<- y
     s <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) s <<- solve
   getsolve <- function() s
   list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Solve for x the inverse of the parameter matrix
## in the equation a %*% x = b where
## a is the parameter matrix 
## b is is the identity matrix of a 
## the solution is only calculated if it has not yet been solved already
## otherwise it is retrieved from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- attr(x,"solve")
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}
