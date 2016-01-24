## First function makes cache to store inverse of a matrix and second function checks if 
## cache matrix has inverse or not. if yes, it gets the cache data or else it calculates inverse.

## This function makes cache matrix

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
      x<<-y
      i<<-NULL
    }
    get<-function()x
    setinverse<-function(solve) i<<-solve
    getinverse<-function() i
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function gets inverse of the matrix from cache matrix

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
           message("getting cache data")
           return(i)
  
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
}
