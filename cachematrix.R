
# This function returns a list that contains properties of the supplied matrix.
# Elements of this list are set, get, setinver, and getinver

makeCacheMatrix <- function(x = matrix()) {
  # initally set inverse to null, because we do not know it
  inver <- NULL
  
  # the set
  set <- function(y)
  {
    x <<- y
    inver <<- NULL
  }
  
  # get the value of the matrix (we set x = matrix() above)
  get <- function() x # so this returns a function type and the X matrix function?
  
  setinver <- function(solution) inver <<- solution ## set the inverse value
  getinver <- function() inver ## get our inverse
  
  list(set = set, get = get, setinver = setinver, getinver = getinver) # our list
}


# this function takes a makeCacheMatrix object and either caclcuates the inverse, or if already calcucalted
# returns the chashed calculated value.

cacheSolve <- function(x, ...) {
  
    # x wil be the makeCacheMatrix object
    inver <- x$getinver()  ## check out the inver value of the object
    
    #if inver isn't null, we already know the inverse
    if (!is.null(inver))
    {
      message("getting cached inverse")
      return(inver) # this returns, and we ingnore the rest of the function
    }
    
    data <- x$get() # get the matrix from our object
    inver <- solve(data, ...) # solve the inverse
    x$setinver(inver) # go ahead and save the inverse into the makeCacheMatrix object so we have it for next time
    inver # return the inverse value
}

