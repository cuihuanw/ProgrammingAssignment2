## Matrix inversion is usually a  a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## makeCacheMatrix is a function that creates a special "matrix" 
## object that can cache its inverse.
## similar to the example on coursera "Caching the Mean of Vector"

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL   ## set the inv to NULL as a placeholder for future value
      
      set<-function(y){
            x<<-y
            inv<<-NULL
      }  ## define a function to set the matrix 'x' to a new matrix 'y', and 
         ## reset the inverse 'inv' to NULL 
      
      get<-function()x   
      ## returns the matrix 'x'
      
      setInverse<-function(inverse) inv<<-inverse   ## set the inverse 'inv' to 'inverse'
      
      getInverse<-function() inv  ## returns the inverse 'inv'
      
      list(set=set, get=get,
           setInverse=setInverse,
           getInverse=getInverse)    ## returns the 'special matrix' containing all of the functions just defined

}




## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

##library('MASS') ## load MASS package for ginv() matrix inverseion 
                  ## which works not only for square invertible matrix 
cacheSolve <- function(x, ...) {
      inv<-x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      matrix<-x$get()
      inv<-solve(matrix,...)  
        ##solve(A) works only when A is aquare matrix 
        ##inv<-ginv(mymatrix,...)  this works for all invertible matrices 
      x$setInverse(inv)
      inv
}
