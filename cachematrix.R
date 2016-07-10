## So in this assignmment I am going to utilize a special property of R 
## called Lexical Scoping
## The goal is to cache the inverse of a matrix so that it doesn't 
## have to compute it again

## The following function computes the inverse of a matrix  and caches
## the inverse of it
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## The next function is where Lexical Scoping comes into play
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ## Calculating the inverse
    get <- function() x
    setTheInverse <- function(inverse) inv <<- inverse
    getTheInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## The following function too computes the inverse of a matrix, but first 
## checks if it has already been computed.
## If so, it retrieves it from the cache
## If not, calculates it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  inverse <- x$getTheInverse()
    message("getting cached data")
    return(inverse)
  }
  ## If it is not in the cache, calculates it
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setTheInverse(inverse)
  inverse
}
