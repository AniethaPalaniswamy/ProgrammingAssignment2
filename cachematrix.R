 #This Function takes a matrix and sets the values and
#saves the calculated  in Cache and reuse the cache where its required
###########################################################################
#makeCacheMatrix function creates a special "Matrix", 
#which is really a list containing a function to #set the value of the Matrix
#get the value of the Matrix #set the value of the Inverse Matrix
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

#####################################################################################
#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
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
#############Here we can see the functions being called and using the cache
mat_cal <- matrix(c(20,22,46,55,12,33,32,2,4,89,99,16,7,29,78,64), nrow=4, byrow=T)
mat_cal
mat_invcal <- makeCacheMatrix(mat_cal)# function is used to create  the cached matrix object 
mat_invcal
final_inv <- cacheSolve(mat_invcal)
final_inv # here it calculates or retrives  the inverse of matrix
final_inv <- cacheSolve(mat_invcal)










