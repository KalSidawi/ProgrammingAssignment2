## Put comments here that give an overall description of what your
## functions do
# below functions initiate a special matrix that has the "methods" to store (set)
# and retrieve (get) the matrix itself as well as its inverse.

## Write a short comment describing this function
# this is the constructor function that creates a list of functions
# set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- matrix(NA)
    set <- function(y)
    {
      if (!is.matrix(y) | nrow(y) != ncol(y)) return(errorCondition("input is not a matrix or a non-square matrix"))
      if (identical(y,x)) return(message("set matrix is identical to the stored matrix"))
      x <<- y
      matrixna <- matrix(NA)
      inv <<- matrixna
    }
    get <- function() x
    setinverse <- function(inverse) 
    {
    # add validation condition on inverse
      inv <<- inverse
    }
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# below function primarily calculates and stores or retrieves - if the matrix
# was not changed - the inverse matrix of the matrix that is retrieved
# by the member function !!get!! of the above function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!all(is.na(inv)))
  {
    message("retrieving inverse matrix from cache..")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
