## cacheMatrix() takes an invertible square matrix and returns a list
## of functions that allow one to get or modify or cache 
## its matrix and its inverse

## cacheSolve() takes a cached Matrix Object created by cache Matrix and
## returns its inverse.

##USAGE 
##cacheMatrixObj <- makeCacheMatrix(invertibleMatrix) #Create cacheMatrixObj
##cacheMatrixObj$set(newInvertibleMatrix) #Modify Existing Matrix Object
##cacheMatrixObj$get() #Get the Cached Matrix Object
##cacheMatrixObj$setinv(solve(invertibleMatrix))#Caches inverse of matrix object
##cacheMatrixObj$getinv()#Gets matrix inverse or returns NULL if not calculated

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##USAGE
## In the code caheMatrixObj is the variable x
## cacheSolve(cacheMatrixObj) either returns the cached inverse
## of cacheMatrixObj or calculates caches and then returns the inverse
## Uses the functions cacheMatrixObj$getinv() or cacheMatrixObj$setinv()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i

}