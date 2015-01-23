## Put comments here that give an overall description of what your 
## functions do

## Create a matrix that can cache its inverse 
 
makeCacheMatrix <- function(x = matrix()) { 
inv=matrix(NA,)             ## Create a null matrix
set=function(y){              
x<<-y
inv<<-matrix(NA,)
}
get=function() x
setinverse=function(inverse) inv<<-inverse
getinverse=function()inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


cacheSolve <- function(x,y, ...) { 
## Return a matrix that is the inverse of 'x' 

data=x$get()
if(identical(data,y)){           ## Check If the matrix has changed
 inv=x$getinverse()
 if(!is.na(inv[1,1])){            ## Check if the inverse has been existed
  message("getting cached data")
  return(inv)
  }
 else{
  inv=solve(data,...)
  x$setinverse(inv)
  }
 }
else if(!identical(data,y)){         ## If the matrix has changed, calculate the          
 inv=solve(y,...)                        ## inverse of the new matrix
 x$setinverse(inv)
}
list(inv=inv)
}
