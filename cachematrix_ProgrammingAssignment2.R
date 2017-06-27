## Robert Michaelis

## Put comments here that give an overall description of what your
## functions do - [rmichaelis27] Rather then computing the inverse many times taking longer, 
## these functions cache the inverse quickly. 

## Write a short comment describing this function - [rmichaelis27] creates a matrix object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }

## Write a short comment describing this function - [rmichaelis27] This function computes the inverse of the matrix created by the
## function called makeCacheMatrix. If the inverse has already been calculated
## , then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
  }

