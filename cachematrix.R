## MATRIX INVERSE WITH CACHING
## INSTRUCTIONS: 
##      When using this code, use makeCacheMatrix before cacheSolve!
##      cacheSolve depends on the list created by makeCacheMatrix!

## ____________________________________________________________

## makeCacheMatrix: Creates a special "matrix" object (it's actually a list)
## that can cache its inverse

## makeCacheMatrix is home to four functions: 
##   set - sets a matrix into cache
##   get - gets the cached matrix
##   setInverse - sets the computed inverse matrix into cache
##   getInverse - gets the already cached inverse matrix

makeCacheMatrix <- function(SavedMatrix = matrix()) {
        ## Resets the inverse of the matrix because it's not yet computed
        ## Thus, whenever a new matrix is cached, the cached inverse resets 
        myInverse <- NULL
        
        set <- function(MatrixToCache){
                SavedMatrix <<- MatrixToCache
                myInverse <<- NULL

        }
        
        get <- function() SavedMatrix
        setInverse <- function(computedInverse) myInverse <<- computedInverse
        getInverse <- function() myInverse
        
        ## End result: a list of functions for the cached matrix
        list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve will retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
        ## Checks if x and its inverse is already cached
        myInverse <- x$getInverse()
        if(!is.null(myInverse)) {
                message("You're in luck! Found cached inverse!")
                return(myInverse)
        }
        
        ## Gets the cached matrix
        data <- x$get()
        
        ## Return a matrix that is the inverse of the cached matrix
        myInverse <- solve(data)
        
        ## Caches the solved inverse of the matrix
        x$setInverse(myInverse)
        myInverse
        
}
