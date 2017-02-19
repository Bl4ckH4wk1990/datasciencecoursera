# The following functions cache the inverse I of a matrix X.
# This avoids recomputing I unless the original matrix X has changed.

# The first function below creates a cached inverse matrix. 4 functions allow
# to modify (set) or get (get) the value of the to be inverted matrix X and
# recalculate (setsolve) or get (getsolve) the value of the inverted matrix I. 
# When the function to set the value of X (set) is called,  the matrix I
# is no longer the correct inverse of X and, therefore, it is set to NULL.


makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(Y) {
    x <<- Y
    I <<- NULL
  }
  get <- function() x
  setsolve <- function(Y) I <<- Y
  getsolve <- function() I
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# The next function returns the inverse of a matrix that was created by
# the previous function (makeCacheMatrix). If the inverse I of X
# has not been calculated yet, or the matrix X changed recently 
# , in both cases I = NULL, it  recalculates the inverse of X. Otherwise, it
# returns the cached value of the inverse (X$getsolve).

cacheSolve <- function(x, ...) {
  I <- x$getsolve()
  if (is.null(I)) {
    message("Output is computed inverse")
    data <- x$get()
    I <- solve(data, ...)
    x$setsolve(I)
  } else {
    message("Output is cached inverse")
  }
  return(I)
}

# TEST THE ABOVE FUNCTIONS:

# Step 1: Create a CacheMatrix:
x <- makeCacheMatrix()
set.seed(1)
x$set(matrix(runif(16, -1, 1), 4))

# Step 2: Runing it the first time, cacheSolve() computes the inverse
cacheSolve(X)

# Step 3: Running it the second time, cacheSolve() retrieves the cached inverse
cacheSolve(X)

# Step 4: Change matrix X and see how cacheSolve() recomputes the inverse
x$set(matrix(runif(16, -1, 1), 4))
cacheSolve(X)
