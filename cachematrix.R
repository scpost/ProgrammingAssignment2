## 4/25/2014 first draft 
## check push from local directory


## makeCacheMatrix defines four functions to 
## handle creation and caching of inverted matrices

## get matrix creates matrix

## set matrix caches matrix

## get inverted matrix retrieves inverted matrix from cache

## set inverted matrix calculates inverse of matrix and
##     caches it



makeCacheMatrix <- function(x = matrix()) {
      
      
      mtx <- NULL
      
      setMatrix <- function(y) {
            x <<- y
            mtx <<- NULL
      }
      getMatrix <- function() x
      
      setInvertedMtx <- function(mtx) mtx <<- matrix()
      
      getInvertedMtx <- function() mtx
      
      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setInvertedMtx = setInvertedMtx,
           getInvertedMtx = getInvertedMtx)

}


## cacheSolve looks for inverted matrix in cache and
## if found returns inverted matrix
## if not found in cache, solve() is called to 
## calculate inverse


cacheSolve <- function(x, ...) {
      
      ## check cache for the inverse of 'x'
      
      mtx <- x$getInvertedMtx()
      if(!is.null(mtx)) {
            
            return(mtx)
      }
      
      ## if not found, get matrix and invert
      ## add inverted matrix to cache
      
      data <- x$getMatrix()
      mtx <- solve(data)
      x$setInvertedMtx(mtx)
      mtx
}
