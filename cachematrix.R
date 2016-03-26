
## the functions used below would create a special matrix object and computes its inverse and also caches it for later use.
## It is assumed that matrices provided will be inversible(square matrix).

## makeCacheMatrix- accepts a matrix and returns a list where each element is a function to operate on the object.

makeCacheMatrix <- function(matx = matrix()) {
   
  cacheInv <- NULL   ## cached inverse
 
  ## update the cached matrix if it has changed. 
  setMatrix <- function(newMat= matrix()){  
     matx <<- newMat
     cacheInv <<- NULL
  } 
  
  ## retreive the matrix
  getMatrix <- function() { matx }
  
  ## update the cached inverse
  setInverse <- function(inv= matrix() ){
     cacheInv <<- inv
  }
  
  ## retrieve the cached inverse
  getInverse <- function() { cacheInv }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse)
}



## computes the inverse of the matrix, caches it / retreives the cached inverse.
## arguments - matList : matrix object created through MakeCacheMatrix()
##             mat : source matrix           

cacheSolve <- function(matList, mat=matrix()) {
        
   getInv <- matList$getInverse()
   
   ## return the cached inverse if its available & has not changed
   if( !is.null(getInv)){
     
      if ( all(matList$getMatrix() == mat) ){ 
         message("Getting the Cached Inverse")  
         return(getInv)
      }
      else{
        message("Matrix has changed, updated the change")
        matList$setMatrix(mat) ## update the matrix
      } 
   }
   
   message("Finding the Inverse")
   newInv <- solve(mat)
   matList$setInverse(newInv)  ## update the inverse
   
   newInv ## return the inverse
}
 