## The two functions below computes and caches the inverse of a matrix to reduce time consumption in repeated computation of same inverse.

## The function makeCacheMatrix() caches the matrix and its inverse which can be fetched for later use.
# In short, makeCacheMatrix() returns a list of functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
m <- NULL
  set <- function(y) 
    {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve() computes the inverse of the matrix.
## Under if(!is.null(inv)), the function checks if the inverse is already calculated 
## for the matrix and fetches the cached value if found. If there is no inverse computed already,
## then it will go ahead with computation of inverse.

cacheSolve <- function(x, ...) 
{
inv<- x$getinverse()
  if(!is.null(inv))
  {
   message("Getting the cached inverse of the matrix")
   return(inv)
  }
  data<- x$get()
  inv<- solve(data)
  x$setinverse(inv)
  inv
}
