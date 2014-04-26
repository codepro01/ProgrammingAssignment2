## This function would create a Cached Matrix instance. This instance intenally is represented as a list
## withs elements as setter for a matrix, getter for a matrix, setter for inverse and getter for inverse of matrix
## values are set using these methods exposed

makeCacheMatrix <- function(x = matrix()) {
  
  #
  invMat <- NULL;
  #setter to set the matrix value
  set <- function(y)
  {
     x<<-y
     
     #ensure if new matrix is set old inverse value is set to NULL
     invMat<<-NULL
  }
  
  
  #getter to get the stored matrix
  get <-function() x
  
  
  #setter to set the inverse of the matrix
  setInverse <-function(inv_mat)
  {
    invMat<<- inv_mat;
  }
  
  #getter of the inverser matrix if stored
  #if not stored NULL would be returned
  getInverse<-function() invMat;
  
  list(set=set, get=get,setInverse=setInverse, getInverse=getInverse);
}


## This function would first validate if inverse of the matrix passed as input
## is already computed. If the input is computed return the cached value , if not
## inverse Matrix would be computed and stored.

cacheSolve <- function(x, ...) {
  
  #Check if the input special matrix has its inverse computed 
  m<-x$getInverse();
  
  #return computed inverse if available
  if(!is.null(m))
  {
    message("Inverse of matrix already cached, returning value");
    return (m);
  }
  
  #compute inverse and update the special matrix to store the cached 
  #inverse
  m<-solve(x$get());
  x$setInverse(m);
  m
}
