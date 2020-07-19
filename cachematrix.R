##put comments here that give an overall description of what your
##function do

##There are two functions makeCacheMatrix, makeCacheMatrix
##makeCacheMatrix consists of set,get, setInverse, getInverse
##library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                     #initializing inverse as NULL
  set <-function(y){
                x <<- y
                inv <<- NULL
  }
         get <- function() x        #function to get matrix
         setinv <- function(inverse) {inv <<- inverse}
         getinv <- function() 
                        inv <- ginv(x)
                        inv %%x     #function to obtain inverse of the matrix
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  
}

##write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(x, ...)    ##gets cache data
  {
  inv <- x$getinv()
  if(!is.null(inv))            #checking whether inverse is NULL
  {
    message("getting cached data!")
    return(inv)                ##return inverse value
    
  }
  kazmi <- x$get()
  inv <- solve(kazmi,...)       #calculates inverse value
  x$setinverse(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
