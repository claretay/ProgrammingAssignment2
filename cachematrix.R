## Put comments here that give an overall description of what your
## functions do

#### There are 2 functions. the first creates a matrix object that can cache its
#### inverse. The second calculates the inverse of the matrix created by the 
#### first function.


## Write a short comment describing this function

#### makeCacheMatrix creates a special 'matrix' which is a list containing 
#### a function to 
#### 1: set the values of the matrix
#### 2: get the values of the matrix
#### 3: set the values of the matrix inverse
#### 4: get the values of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

#### cacheSolve calculates the inverse of a "matrix" created with 
#### makeCacheMatrix
#### Return a matrix that is the inverse of 'x'
#### uses 'solve()' [http://www.statmethods.net/advstats/matrix.html], 
#### so only works for a symettric, positive definite square matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
