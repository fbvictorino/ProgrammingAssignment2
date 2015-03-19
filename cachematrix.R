## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix "x"
## It returns a special matrix that is really a list of functions
## that sets the value of x, gets the value of x,
## sets the inverse value, gets the inverse value

makeCacheMatrix <- function(x = matrix()) {
    #initialize the inverse variable to Null
    inverse <- NULL
  
    #set function that transfers a value to the matrix
    set<-function(y){
        x<<-y
        inverse <<-NULL
    }
  
    #get function that retrieves the cached value for the matrix
    get <- function() x
  
    #sets the cached value of the inverse
    setinverse <- function(inverse_new) inverse <<- inverse_new
  
    #gets the cached value of the inverse
    getinverse <- function() inverse
  
    #returns the list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function returns the inverse of the special matrix
## The special matrix must be made through the makeCacheMatrix function
## Initial value of the matrix should be set by the "$set" function

cacheSolve <- function(x, ...) {
  
    #gets the cached value for inverse
    inverse<- x$getinverse()
  
    #if there's a cached value, it returns the matrix inverse
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
  
    #if inverse is Null, get the matrix values set
    data <- x$get()
  
    #solve for the inverse of the matrix
    inverse <- solve(data,...)
  
    #set the cached value of the inverse
    x$setinverse(inverse)
  
    #return the inverse matrix
    inverse
}
