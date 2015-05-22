# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

     m<-NULL
     set<-function(y){
       x<<-y
       m<<-NULL
     }
     get<-function() x
     setmatrix<-function(solve) m<<- solve
     getmatrix<-function() m
     list(set=set, get=get,
        setmatrix=setmatrix,
	   getmatrix=getmatrix)

}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

## This function works for invertible matrix only.

cacheSolve <- function(x, ...) {
     m<-x$getmatrix()
     if(!is.null(m)){                 # Check if inverse of matrix is cached
       message("getting cached data")
       return(m)                      # Exit Program
     }
     matrix<-x$get()                  # Put matrix in variable "matrix"
     m<-solve(matrix, ...)            # Compute Inverse of the matrix
     x$setmatrix(m)                   # Cache the Inversed matrix
     m				      # Return Inversed matrix

}
