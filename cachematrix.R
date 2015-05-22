## create a special matrix with makeCacheMatrix. This returns a list of functions.
## Then you may solve the cache matrix using cacheSolve, which will retrieve
#  the inverse w/o recomputing if it was already been computed

## Creates a vector of functions on the given matrix.
## Provides inverse caching
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             #inverse starts empty
    set <- function(y = matrix()) {
        x <<- y             #reassign matrix in parent env
        inv <<- NULL        #must calc inverse again if reset     
    }
    get <- function() {
        x                   #just return matrix
    }
    setinverse <- function(inverse) {
        inv <<- inverse     #set inverse
    }
    getinverse <- function() {
        inv                 #return the inverse
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## solve the matrix provided from the first method, checking for a cached value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()   #call getinverse -> returns NULL if not cached
    if(is.null(inv)) {
        data <- x$get()     #get the value of matrix
        inv <- solve(data)  #returns inverse
        x$setinverse(inv)   #save inverse to vector obj
        inv                 #return inverse to user
    }
    else {
        message("getting cached inverse")
        inv
    }
}
