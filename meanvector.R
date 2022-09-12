# Function to provide the mean value for each column of a matrix
# It has a constructor function that prepares the object to set / get the list /
# matrix and set / get the mean
#

contruct_mean_vector <- function(x = numeric())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cache_mean <- function(x)
{
  m <- x$getmean()
  if (!is.null(m))
  {
    message("getting mean from cache..")
    return(m)
  }
  data <- x$get()
  m <- mean(data)
  x$setmean(m)
  m
}