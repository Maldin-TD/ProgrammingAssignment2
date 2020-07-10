## The makeCacheMatrix() function generate a matrix and generate and find its inverse.
##The cacheSolve() function checks if the inverse matrix exists and if not,it returns a inverse matrix

## The function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix=NULL
  set=function(y)
  {
    x<<-y
    inverseMatrix=NULL
  }
  get=function(){x}
  setInv=function(solve){inverseMatrix<<-solve}
  getInv=function(){inverseMatrix}
  list(set=set,get=get,setInv=setInv,getInv=getInv)

}


## The function computes the inverse of a special matrix returned by the makeCacheMatrix above.
##If the inverse has already been calculated then retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       
  inverseMatrix=x$getInv()
  
  ##check if the inverse matrix already has been generated
  ##If yes,it prints a message 
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  myMatrix=x$get()
  ##generate the inverse matrix
  
  inverseMatrix=solve(myMatrix,...)
  x$setInv(inverseMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix
}
