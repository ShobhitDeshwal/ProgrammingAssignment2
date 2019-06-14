## There are two functions, one is for making a matrix and the other is for 
## inverting the square matrix and caching it.

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  ## we are assigning a value null to the inv mat as a net mat is passed in argumnt
  inverse_matrix <- NULL
  ## set the new matrix and assign null to inverse_matrix as it is new matrix
  set <- function(y){
        x <<-y
        inverse_matrix <<- NULL
  }
  ## getting the vnew matrix
  get <- function()x
  
  ## give the calue of argument to the inverse_matrix
  setinv_mat <- function(inver_mat){
    inverse_matrix <<- inver_mat
  }
  ## get the value of inverse_matrix stored
  getinv_mat <- function()inverse_matrix
  
  ## return a list with variable name as function names so that we can use $ 
  ## operation in cacheSolve()
  list(set = set,
       get = get,
       setinv_mat = setinv_mat,
       getinv_mat = getinv_mat)
}

## this funciton will calculate and cache the inverse of the matrix passed as an
## argument to it.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      ## get the inverse of matrix if already cache(if no new matrix is passed)  
    inverse <- x$getinv_mat()
    ## if the inverse is not null , return the same to the function
    if(!is.null(inverse)){
      message("getting cached inverse matrix")
      return(inverse)
    }
    ## else invert the matrix and return the inverted matrix
    data <- x$get()
    inverse <- solve(data)
    x$setinv_mat(inverse)
    inverse
    
    
    
    
}

