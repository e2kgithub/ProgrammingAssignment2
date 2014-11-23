
## 1. makeCacheMatrix: This function creates a special "matrix" object ## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     
     # sets the value of m to NULL (provides a default if cacheSolve 
     # has not yet been used)
     m<-NULL
     
     # sets the value of y to NULL (provides a default if cacheSolve 
     # has not yet been used)
     y<- NULL  #?not sure about this?
     
     #set the value of the matrix
     set<-function(y){
          
          # caches the inputted matrix so that cacheSolve can check 
          # whether it's changed (note this is within the setmatrix function)
          x<<-y
          
          #sets the value of m (the matrix inverse if used cacheSolve) to NULL
          m<<-NULL
     }
     
     
     get<-function() x
     setmatrix<-function(solve) m<<- solve
     getmatrix<-function() m
     
     
     # create list of the four functions
     list(set=set, get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}


## 2. cacheSolve: This function computes the inverse of the special 
## matrix returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
     m<-x$getmatrix()
     
     # check to see if cacheSolve has been run before
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     
     # check that matrix hasn't changed, and if it hasn't, send a message and returns the cached matrix
     matrix<-x$get()
     m<-solve(matrix, ...)
     x$setmatrix(m)
     return(m)
     
     # otherwise 
     # run the getmatrix function to get the value of the 
     # input matrix
     y <- x$getmatrix() 
     
     # run the setmatrix function on the input matrix to cache it
     x$setmatrix(y) 
     
     # compute the value of the inverse of the input 
     matrixm <- solve(y, ...) 
     # run the setinverse function on the inverse to cache the inverse
     x$setinverse(m) 
     # return the inverse
     return(m)
}


