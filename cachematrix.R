## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).
## The following two functions makeCacheMatrix and cacheSolve can be used to inverse a existing matrix and
## in case the inverse matrix data are needed a second time then a cache will be
## passed and not the inverse computetd a second time.



## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the nverse matrix

## makeCacheMatrix can be called like this x <- makeCacheMatrix()
## x <- makeCacheMatrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE))

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<-  solve(x)
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse matric has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matric of the data and sets the value of the inverse matric
## in the cache via the setinverse function.

## the function can be called like this
## first time
## > cacheSolve(x)
##       [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
## second time
## > cacheSolve(x)
## getting cached data
##       [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0

cacheSolve <- function(x, ...) {

       i <- x$getinverse()
       if(!is.null(i)) {
               message("getting cached data")
               return(i)
       }
       data <- x$get()
       i <- solve(data)
       x$setinverse(i)
       ## Return a matrix that is the inverse of 'x'
       i
}
