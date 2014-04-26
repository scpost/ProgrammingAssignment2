## Functions based on those outlined in  
##     rdpeng/ProgrammingAssignment2/README.md



## makeCacheMatrix() accepts matrix object as argument 
## and defines four functions to handle
## creation and caching of inverted matrices

## get()
## set() 
## getInverse() 
## setInverse()

 


makeCacheMatrix <- function(x = matrix()) {
      
      
      m <- NULL
      
      
      
      set <- function(y) {        # set value of matrix object
            x <<- y
            m <<- NULL
      }
      
      
      
      get <- function()  {        # retrieve value of matrix object       
            x
      }
      
      
      
      setInverse <- function(inverse) {  # set value of inverted matrix
            
            m <<- inverse
      }      
      
       
      
      getInverse <- function() {  # retrieve value of inverted matrix 
            m
      }
      
      list(set = set, get = get,     # return a list of functions
           setInverse = setInverse,
           getInverse = getInverse)
      
}


## cacheSolve() looks for inverted matrix in cache and
## if found returns inverted matrix
## if not found in cache, solve() is called to 
## calculate inverse of matrix 


cacheSolve <- function(x, ...) {
      
      
      
      m <- x$getInverse()     # look for inverted matrix in cache
      if(!is.null(m)) {       # if found, return inverted matrix
            
            return(m)
      }
      
      ## if not found, process matrix 
      
      
      data <- x$get()         # retrieve matrix
      m <- solve(data, ...)   # calculate inverse 
      x$setInverse(m)         # add inverse to cache
      m                       # return inverse matrix 
}