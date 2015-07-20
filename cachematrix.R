## allow for caching and retrieving the inverse of a matrix

## builds and stores a matrix, caches and retrieves the supplied inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
  ## define the object
  m <- NULL
  
  ## set a new value for the stored matrix
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  ## get the current version of the stored matrix
  get <- function() 
  {
    x
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) 
  {
    m <<- inverse
  }
  
  ## get the stored inverse of the matrix
  getmean <- function() 
  {
    m
  }
  
  ## return list of functions
  list(
    set        = set, 
    get        = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## check for cached inverse: if found, return; if not, calculate and store.
cacheSolve <- function(x, ...) 
{
  ## check if the inverse is already stored
  m <- x$getInverse()
  
  ## if so, return it
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  ## if not, retrieve the stored matrix
  data <- x$get()
  
  ## calculate the inverse
  m    <- solve(data, ...)
  
  ## store the inverse
  x$setInverse(m)
  
  ## return the inverse
  m
}