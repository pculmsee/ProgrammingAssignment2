## Create an object that allows you to cache the results of a matrix inversion
## and use that instead of just calling solve on the matrix
## Usage: Pass a matrix into makeCacheMatrix then call cacheSolve with the result.
## The first time it will calculate the inverse and store it in the getinverse function
## of the object for makeCacheMatrix.  The next time it just looks for that.

## Creates an object with get and set functions. get() returns the original input matrix,
## set() stores the input matrix
## setinverse() stores the inverted matrix
## getinverse() returns the stored inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        ## construct a cacheable object including the matrix to be inverted
        i<-NULL
        set <- function(y){
                x<<- y
                i<<-NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(solve){
                i<<-solve
        }
        getinverse <- function(){
                i
        }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of a matrix passed in as part of a makeCacheMatrix object
## The first time it runs, the inverse is calculated and stored in the makeCacheMatrix object.
## The second time it returns the result that was stored in the first run

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return (i)
        }
        data <- x$get()
        i<-solve(a=data,...)
        x$setinverse(i)
        i
}
