## Between them makeCacheMatrix and cacheSolve save the time required for
## computing inverse of a matrix by caching it. Matrix inversion is time 
## consuming. So it is beneficial to cache the result and return it, rather
## than repeatedly compute it, if the matrix to be inverted is the same. 
##  
## The special function makeCacheMatrix generates four other functions 
## through which the matrix and its inversion can be set and retrieved. 
##
makeCacheMatrix <- function(x = matrix()) {      ## Argument is a matrix.
                                                 ##
  inv <- NULL                      ## Initializing inverse to NULL value.
                                   ##
  set <- function(y) {             ## After calling makeCacheMatrix once 
                                   ## if more Matrices related queries   
                                   ## are sought to be passed through,    
                                   ## this set function is used.
                                   ##
    x <<- y                        ## y is the input for the new call 
                                   ## received through the argument of 
                                   ## calling function. It is passed to  
                                   ## the variable of the main function 
                                   ## in the parent environment.
                                   ##
    inv <<- NULL                   ## When new data is received, the  
                                   ## inverse stored is reset to NULL  
                                   ## in the parent environment.
  }
                                   ##
  get <- function() x              ## x value from makeCacheMatrix is   
                                   ## passed to calling function. This  
                                   ## function receives no external 
                                   ## arguments. 
                                   ##
  setInv <- function(fmOtherFun) inv <<- fmOtherFun  
                                   ##
                                   ## fmOtherFun is the inverse matrix 
                                   ## computed by cacheSolve function 
                                   ## and being passed to the variable inv 
                                   ## in makeCacheMatrix (note <<- )  
                                   ##
  getInv <- function() inv         ## This function passes the value stored  
                                   ## undervariable name 'inv' in the parent 
                                   ## environment of makeCacheMatrix to 
                                   ## the calling function.
                                   ##
  list(set = set, get = get,       
       setInv = setInv,
       getInv = getInv)
                                   ## The output of the parent function as a list 
                                   ## ensures $names are accessible to calling functions
}


## The cacheSolve function interacts with the makeCacheMatrix function
## It queries the current inverted matrix and if it is NULL, computes 
## and returns the inverted matrix. If this current value is not NULL,
## this function obtains the previous inverted matrix and returns. 
## The output of this matrix is always the inverted matrix. 

cacheSolve <- function(x, ...) {    ## The argument of this function is 
                                    ## the variable name to which the 
                                    ## function makeCacheMatrix 
                                    ## with valid arguments is assigned. 
                                    ##
  inv <- x$getInv()                 ## This line gets the inverse from  
                                    ## x$getinv() function to see if this 
                                    ## is NULL
                                    ##
  if(!is.null(inv)) {               ## Checking to see if the inverse  
                                    ## received is not a NULL
                                    ##
    message("getting cached data")  ## If the received inverse is not a 
                                    ## NULL, there is no need to compute.
                                    ## So a message is put out. 
                                    ##
    return(inv)                     ## The cached data is returned as the 
                                    ## inverse by cacheSolve. 
  }
  data <- x$get()                   ## Else, when stored inverse is not a  
                                    ## NULL, new inverse computation is 
                                    ## initiated. The new input value through
                                    ## x$get() is assigned to variable 'data'
                                    ##
  inv <- solve(data)                ## solve is a R function that returns 
                                    ## the inverse of a matrix
                                    ##
  x$setInv(inv)                     ## The newly computed value is passed to 
                                    ## makeCacheMatrix through the x$setInv
                                    ## function with inv as the argument 
                                    ##
  inv                               ## Having computed the new inv value 
                                    ## cacheSolve returns this new value
}







