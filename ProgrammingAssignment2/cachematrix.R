## This file is a solution to Programming Assignment 2 of the 
## Programming in R course on Coursera as part of the John 
## Hopkins Data Science Specialization.
##
## This file consists of two programs used to create a special
## type of matrix that can store inverses in a cache such that
## once an inverse is caluclated doing the same inverse again
## will be returned from the cache causing an improvement in 
## efficiency.
##
## Author: Oskari Rautiainen (oskari.rautiainen@gmail.com)
## Last Modified: 08/09/2016

## This function outputs a special matrix from an existing matrix
## passed as input that allows you to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function can be called on the matrix created by makeCacheMatrix
## to calculate the inverse of the matrix. This is the function that
## stores and retrieves the inverse from a cache if available.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

## Sample usage:
##
## B<-matrix(c(6,9,5,8),nrow=2,ncol=2)
## x<-makeCacheMatrix(B)
## cacheSolve(x)
## [,1]      [,2]
## [1,]  2.666667 -1.666667
## [2,] -3.000000  2.000000
## > cacheSolve(x)
## getting cached data
## [,1]      [,2]
## [1,]  2.666667 -1.666667
## [2,] -3.000000  2.000000
