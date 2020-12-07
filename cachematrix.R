## These two functions help to make Cached matrix, finding its mean and
## returning inverse of the matrix 

## Creating cache matrix which can be used to get its inverse

makeCacheMatrix <- function(x = matrix()) { 
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) m <<- inverse
   getInverse <- function() m
   
   list(set = set, get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}

## Returning inverse of the matrix

cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   if(!is.null(m)){
      message("getting cached data")
      return(m)
   }
   data <- x$get
   m <- solve(data)
   x$getInverse(m)
   m
}