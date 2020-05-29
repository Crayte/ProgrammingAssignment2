## This function caches the inverse of a matrix and assigns the value 
## to an object that may be used in an environment different from the current

makeCacheMatrix <- function(x = matrix()) {
      j <- NULL
      set <- function(y){
            x <<- y
            j <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) j <<- inverse
      getInverse <- function() j 
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## This function calculates the inverse of the matrix, checking to see if the
## inverse has already been calculated. If it has, it will 'get' the inverse
## from the cache and skip the potentially time-consuming calculation. If the 
## cache is null, it calculates the inverse of the matrix and assigns it to 
## 'j' via the the setInverse function

cacheSolve <- function(x, ...) {
      j <- x$getInverse()
      if(!is.null(j)){
            message("getting cached data")
            return(j)
      }
      mat <- x$get()
      j <- solve(mat,...)
      x$setInverse(j)
      j
}
