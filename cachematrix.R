## Put comments here that give an overall description of what your
## functions do: ############It takes a matrix and sets the values and
#saves the calculated  in Cache and reuse the cache where its required

## Write a short comment describing this function
############This function creates a special "matrix" object that can cache its inverse
############ The first function, makeCacheMatrix creates a special "Matrix", 
#which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#set the value of the Inverse Matrix
#get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y)
  {
    x <<- y
    mat <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) mat <<- inverse 
  getinverse <- function() mat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
#############This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m_t<-x$getinverse()
  if(!is.null(m_t)) {
    message("getting cached data")
    return(m_t)
  }
  data <- x$get()
  m_t <- solve(data, ...)
  x$setinverse(m_t)
  m_t  
}










