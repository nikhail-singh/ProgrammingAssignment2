## Course: Coursera R programming
## Date: October 10th 2021
## Description: The two functions in this file allow a user to cache the
## inverse of a matrix. This is useful because computing the inverse can
## sometimes be a costly operation. The first function creates a special
## version of the matrix that can cache its inverse while the second 
## checks if the inverse has been cached. If yes it returns the inverse
## if no it computes, stores and then returns the inverse.

## This function creates a special version of a matrix that
## can have its inverse cached. It contains 4 sub-functions that work the same
## way they worked for the vector function given as an example

makeCacheMatrix <- function(x = matrix()) { ##return the special matrix
  m <- NULL ## initialize the value of the inverse as NULL
  
  ##the set function sets the value of the inverse to what is computed
  set <-function(y) {
    x <<- y ## sets x to the value of the matrix in the parent environment
    m <<- NULL ## reset m to NULL if there is a new matrix
  }
  get <- function() x ## returns the matrix (not the inverse)
  setinverse <- function(inverse) m <<- inverse ## gets the value of the inverse
  getinverse <- function() m ## sets the value of the inverse
  
  ## return a list of functions we can reference using the $operator
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special version of a matrix
## created using the makeCacheMatrix function. It first checks if the 
## inverse was already created and if so returns that inverse. If not
## it computes the value, sets the value in cache and then returns it.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { ## if the inverse is already in the cache
    message("getting cached data") ## inform user where the value is from
    return(m) ## return the inverse from the cache
  }
  ##if the inverse is not in the cache
  message("computing inverse") ## inform user
  data <- x$get() ## get the data from the provided special Matrix
  m <-solve(data,...) ## solve for the inverse
  x$setinverse(m) ## set the inverse in the cache to the calculated value
  m ## return the computed inverse
  }
