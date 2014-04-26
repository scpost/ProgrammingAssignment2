## Functions based on those outlined in  
##     rdpeng/ProgrammingAssignment2/README.md

## 4/25/2014 upgrade of functions in makeCacheMatrix 



## makeCacheMatrix defines four functions to 
## handle creation and caching of inverted matrices

## get  returns function object 

## set  

## getInverse  retrieves inverted matrix 

## setInverse  calculates inverse of matrix and
##     caches it



makeCacheMatrix <- function(x = matrix()) {
      
      
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      setInverse <- function(inverse) m <<- inverse
      
      getInverse <- function() m
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}


## cacheSolve looks for inverted matrix in cache and
## if found returns inverted matrix
## if not found in cache, solve() is called to 
## calculate inverse


cacheSolve <- function(x, ...) {
      
      ## check cache for the inverse of 'x'
      
      m <- x$getInverse()
      if(!is.null(m)) {
            
            return(m)
      }
      
      ## if not found, get matrix and invert
      ## add inverted matrix to cache
      ## return inverted matrix
      
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}