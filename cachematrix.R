## Week 3 - Assigment2
## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {# input x will be a vector
  
  m <- NULL    #  m will be our 'Inverse Matrix' and 
               #  it's reset to NULL every time makeCacheMatrix 
               #  is called
  
  # note these next four functions are defined but not run 
  # when makeCacheMatrix is called.
  # instead, they will be used by cacheSolve() to get values 
  # for x or for m (Inverse Matrix) and for setting the 
  # Inverse Matrix.  These are usually called object 'methods'
  
  # this function reset the object created when 
  # makeCacheMatrix has been called
  set <- function(y) { # takes an input matrix
    x <<- y            # saves the input matrix 
    m <<- NULL         # resets the mean to NULL, basically what 
                       # happens when a new object is generated.
  }
  
  # this function returns the value of the original matrix
  get <- function() { x }   
  
  # this is called by cacheSolve() during the first cacheSolve()
  # access and it will store the value using superassignment
  setInverseMatrix <- function(v_matrix)  { m <<- v_matrix }
  
  # this will return the cached value to cacheSolve() on
  # subsequent accesses
  getInverseMatrix <- function() { m } 
  
  
  # This is accessed each time makeCacheMatrix() is called,       
  # that is, each time we make a new object.  This is a list of 
  # the internal functions ('methods') so a calling function
  # knows how to access those methods. 
  list(get = get,                 
       setInverseMatrix = setInverseMatrix,  
       getInverseMatrix = getInverseMatrix)  
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x,...) { # the input x is an object created 
                                # by makeCacheMatrix
  
  m<-x$getInverseMatrix()   # accesses the object 'x' and gets 
                            #the value of the inverse matrix
  
  if(!is.null(m)) {         # if inverse matrix was already 
                            # cached (not NULL) ...
    message("getting cached data") # send this message to the console
    return(m)                      # and return the inverse matrix 
                                   # "return" ends the function. 
  }
  
  # we reach this code only if x$getInverseMatrix() returned NULL
  
  data <- x$get()             # get the 'original' the matrix
  
  m <- solve(data, ...)       # if m was NULL then we have 
                              # to calculate the inverse matrix
  
  x$setInverseMatrix(m)       # store the calculated inverse matrix 
                              # value in x (see setInverseMatrix() 
                              # in makeInverseMatrix
  
  m                           # return the inverse matrix to the 
                              # code that called this function 
}
