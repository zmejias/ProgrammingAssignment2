
## This function receives a matrix and returns a list of functions 
## with the following purpose:
##
## - set: set value of the matrix in parent environment, and set 
##   stored inverse to Null in parent environment
##
## - get: get value of the matrix in the evaluating environment
##
## - setInverse: set value of inverse in parent environment
##
## - getInverse: get value of inverse in evaluating environment 

makeCacheMatrix <- function(data_matrix = matrix()) {
  
   stored_inverse <- NULL
   
   set <- function(y) {
     data_matrix <<- y                            
     stored_inverse <<- NULL                     
   }
   
   get <- function() {
     return (data_matrix)
   }
   
   setInverse <- function(sent_calculated_inverse){
     stored_inverse <<- sent_calculated_inverse
   }
   
   getInverse <- function() {
     return(stored_inverse)
   }
   
   
   list(set = set, get = get,                      ## Returns the list of functions to create
        setInverse = setInverse,                   ## the cached Matrix 
        getInverse = getInverse)

}


## This Function receives the list of function and 
## returns the inverse of a matrix

cacheSolve <- function(madeMatrix, ...) {
      
  local_inverse <- madeMatrix$getInverse()         ## assigns the inverse of the madeMatrix
                                                   ## environment to the inverse of this environment 
  
  if(!is.null(local_inverse)) {
    message("getting cached data")                 ## checks if the inverse of the madeMatrix 
    return(local_inverse)                          ## environment has been calculated before
  }
  else {
    local_data <- madeMatrix$get()
    local_inverse <- solve(local_data, ...)
    
    madeMatrix$setInverse(local_inverse)
    return(local_inverse) 
  }
  
  
}

