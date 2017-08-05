## Put comments here that give an overall description of what your
## functions do
## The pair of functions makeCacheMatrix and  cacheSolve allow to cache the calculation of an invertible matrix and cache it so that the calculation is only done first time

## Write a short comment describing this function
#' Title makeCacheMatrix
#'
#' @param x : the invetible matrix for which to calculate and cache the inverse
#'
#' @return : an object with the function get and set to get and set the matrix, and the function getinverse and setinverse to allow storing the inverse in cache
#' @export
#'
#' @examples makeCacheMatrix(matrix(c(5,-2,3,7), nrow = 2, ncol = 2))
makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setinverse  <- function(inverse ) m <<- solve
          getinverse  <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Write a short comment describing this function
#' Title cacheSolve
#'
#' @param x : the returned object from makeCacheMatrix
#'
#' @return : the inverse of the matrix by calculation first time the inverse and get the inverse from cache next times
#' @export
#'
#' @examples cacheSolve(makeCacheMatrix(matrix(c(5,-2,3,7), nrow = 2, ncol = 2)))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}
