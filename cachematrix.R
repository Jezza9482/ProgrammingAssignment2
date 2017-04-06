## The following two functions use lexical scoping to find the inverse of an 
## invertible matrix "x". They do this by first checking if the inverted 
## matrix "Inv" already exists in the workspace for the given "x". If so, 
## the cached matrix "Inv" is returned. Otherwise, it is calculated using the
## solve function and stored as "Inv" in cache for future use, then returned.

## makeCacheMatrix
## This function takes an invertible matrix, "x", as input and creates a 
## "special vector" or list of functions which can be used by the cacheSolve 
## function. This creates a cached matrix. The function list includes:
##       *set       -sets the values of the matrices "x" and "Inv"
##       *get       -gets the values of the matrix "x"
##       *setInver  -sets the values of the inverse matrix "Inv"
##       *getInver  -gets the values of the inverse matrix "Inv"
## The matrix "Inv" is intialised as NULL to set aside memory for the matrix
## and to clear any pre-existing "Inv" values defined by alternate "x" 
## matrices used by makeCacheMatrix/cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
        }
    get <- function() x
    setInver <- function(Inver) Inv <<- Inver
    getInver <- function() Inv
    list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## cacheSolve
## This function takes a "special vector" or list of functions defined by the   
## makeCacheMatrix function and returns the inverse matrix, "Inv", of the 
## original "x" argument of makeCacheMatrix. Firstly, it calls the getInver 
## function from the input list to get any existing value of "Inv". Then, if 
## "Inv" exists in a non-NULL form, it returns the message "retrieving cached 
## data", then retrieves and returns the cached "Inv" matrix, skipping further 
## computation. If "Inv" only exists in a NULL form, then it sets local variable
## "data" to be the original "x" matrix (or a matrix defined by x$set()). It 
## then uses the solve function on "data" to find the inverse matrix and sets it
## locally to "Inv". "Inv" is then set out of the local environment using 
## setInver and is returned.

cacheSolve <- function(x, ...) {
    Inv <- x$getInver()
    if(!is.null(Inv)) {
        message("retrieving cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInver(Inv)
    Inv
}
