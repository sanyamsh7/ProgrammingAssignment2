## Put comments here that give an overall description of what your
## functions do

## function creates a matrix with a function to:
#       1. set matrix
#       2. get matrix
#       3. setInverse of matrix
#       4. getInverse of matrix
## and returns the list of the all the functions

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        setInverse <- function(inverse){
                inv <<- inverse
        }
        getInverse <- function(){
                inv
        }
        list(set = set, setInverse = setInverse, 
             get = get, getInverse = getInverse)
}


## functions checks for the cached inverse of the matrix
## if the inverse is not cached earlier it calculates the inverse
## and cache it for later use.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                print("getting cached inverse")
                return(inv)
        }
        else{
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
                ## Return a matrix that is the inverse of 'x'
                return(inv)
        }
}
