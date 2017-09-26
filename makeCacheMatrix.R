## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## 1. makeCacheMatrix function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) 
{
  inverse <- NULL
  set <- function(n)
  {
    m <- n
    inverse <- NULL
  }
  get <- function() m
  setInverse <- function(solveMatrix) inverse <- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## 2. cacheSolve function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(m, ...) 
{
  ## Returning inverse of matrix m
  inverse <- m$getInverse()
  if(!is.null(inverse))
  {
    message("returning cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data)
  m$setInverse(inverse)
  inverse      
}