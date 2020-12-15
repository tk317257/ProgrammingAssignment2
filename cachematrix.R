## 
## The following functions are intended to reduce run-time/calculations for the inverse of a matrix by storing the 
## calculated inverse of a mtarix object in a cache in memory making it easily accesible for future use
## 
##
#



## makeCacheMatrix creates a special matrix object that can cache its inverse
##      
##      Initializes objects:
##        *  x as function argument defautlt = matrix()
##        *  m as NULL
##
##      Returns a list of four functions: 
##        1. set() can be called to change the matrix object in the funtion
##        2. get() gets the 
##        3. setIM()
##        4. getIM()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setIM <- function(solve) m <<- solve
    getIM <- function() m
    
    list(set = set, get = get, setIM = setIM, getIM = getIM)

}


## cacheSolve checks to see if the inverse of the special matrix object is already solved in the cache
## if the inverse of the matrix object has not been solved for in the cache solve for the inverse of the matrix 
## and cache it to memory.  This function is required to set and get information from the makecacheMatrix function  
##
##      Initializes objects:
##        *  x as function argument 
##        *  allow additional arguments to be passed via the ... argument
##
##      Uses the function objects in the list created by makeCacheMatrix to: 
##        1. use the getIM() funtion to asign the value in the cache to m
##        2. test m to see if there is a value in the cache, if there is a value, print a message that the inverse matrix was 
##           and retreive it. 
##        3. if there is no value in the cache, get/retreive the special matrix object using x$get() funtion and asign it to mdata
##        4. use the solve() function to calculate the inverse of the matrix object in mdata and asign it to m
##        5. use the x$setIM function to store the inverse matrix object asigned to m into the cache in memory 
##        6. return the inverse matrix


cacheSolve <- function(x, ...) {
        
    m <- x$getIM()
        
    if(!is.null(m)){
        message("success! getting cached inverse matrix data")
        return(m)
    }
    
    mdata <- x$get()
    m <- solve(mdata)
    x$setIM(m)
    m
}
