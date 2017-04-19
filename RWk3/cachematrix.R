## set function - takes incoming y variable and assigns it globally 
## to the matrix x, and sets inverse var(i) to null globally,
## for first time through inverse it not cached, it's  null

## get function - retrieves the matrix and assigns to get

## setinverse function - once inverse is caluclated, it is
## assigned to i variable globally to be checked

## getinverse function - returns the inverse from globally set variable  

## The makeCacheMatrix function creates a list of functions to be applied to a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  ##First run through the cached value i is null locally
    set <- function(y) {
      x <<- y 
      i <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    ## returns a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Since x is a matrix, uses the associated getinverse function
    ## to retreive the default value and assigns to i
    i <- x$getinverse() 
    
    ## verifies whether the inverse has already been calculated or not
    ## if i is not null then returns i found above
    if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
    }
    data <- x$get() ## retrieves matrix found by running get function and assigns to data variable
    i <- solve(data, ...)  ##computes the inverse and assigns to i
    x$setinverse(i) ## takes computed value of i and assigns to setinverse function
    i  ## returns computed value of i to cacheSolve function
  }
  
