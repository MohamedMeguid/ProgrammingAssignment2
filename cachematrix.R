# Here, 2 functions are defined: makeCacheMatrix, cacheSolve with the aim to:
# calculate the inverse of a matrix and cache it as long as the matrix data is the same

## the makeCacheMatrix function acts as a constructor for the matrix object 
## we would like to calculate its inverse and cache
## It contains 4 functions
## get: returns the matrix data
## getInverse: returns the inverse of the matrix
## set: changes the matrix data
## setInverse: changes the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function(){
        return(x)
    }
    getInverse <- function(){
        return(inv)
    }
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    setInverse <- function(inverse){
        inv <<- inverse
    }
    return(
        list(
            get = get,
            getInverse = getInverse,
            set = set,
            setInverse = setInverse
        )
    )
}


## the cacheSolve function calculates the inverse of a matrix data stored in a makeCacheMatrix object,
## it first checks if there is an inverse already calculated and cached
## if inverse is cached, it returns it, otherwise, it calculates, caches and returns the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Getting cached Inverse")
        return(inv)
    } else {
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        return(inv)
    }
}
