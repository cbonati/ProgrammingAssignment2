## This second programming assignment requires to write an R function 
## that is able to cache potentially time-consuming computations.

## For a very large (square invertible) matrix, it may take too long to compute the inverse,
## especially if it has to be computed repeatedly.

## If the contents of a matrix are not changing, it may make sense to cache
## the value of the matrix so that when we need it again, it can be looked up
## in the cache rather than recomputed.

## This Programming Assignment will take advantage of the scoping rules
## of the R language and how they can be manipulated to preserve state
## inside of an R object.


## 1. The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      ## 1.1 Set the value of the matrix
      set <- function(y) {
            ## The <<- operator which can be used to assign a value to an object
            ## in an environment that is different from the current environment
            x <<- y
            m <<- NULL
      }
      
      ## 1.2 Get the value of the matrix
      get <- function() x
      
      ## 1.3 Set the value of the iverse
      setsolve <- function(solve) m <<- solve
      
      ## 1.4 Get the value of the iverse
      getsolve <- function() m
      
      ## 1.5 List
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## 2. The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      
      ## 2.1 If the inverse has already been calculated (and the matrix has not changed),  
      ## then the cachesolve will retrieve the inverse from the cache. (and thus skip the computation)
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## 2.2 Otherwise, it calculates the inverse of the matrix
      ## and sets the value of the inverse in the cache via the setsolve function.
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}