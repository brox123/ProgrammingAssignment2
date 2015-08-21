## This script consists of two functions: makeCacheMatrix and cacheSolve.

## They are used to solve the inverse of a matrix and store the result for re-usage.

## Use the script in the following way:

## Initiate makeCacheMatrix to load the functions and initial matrix into memory: x <- makeCacheMatrix(YOURMATRIX)

## Get the inverse using cacheSolve(x)

## If the YOURMATRIX changes it has to be input again using x$set(YOURMATRIX)





## makeCacheMatrix creates a list with 4 elements. The elements of this list are functions. The inverse and matrix are stored as variables in this function



makeCacheMatrix <- function(x = matrix()) { ## receive the matrix to be used and store it to a variable
    
    i <- NULL ## Set i to NULL upon initiation
    
    set <- function(y) { ## Create the set function which is used to store the matrix that we want the inverse of, and set the corresponding inverse to NULL since it has not yet been calculated.
        
        x <<- y ## Once set is run it will overwrite x in the parent environment by the received value
        
        i <<- NULL ## i in the parent environment is overwritten once a new variable is set, because the inverse has not yet been calculated
        
    }
    
    get <- function() x ## Create the get function to return the matrix that is stored in this function
    
    setinv <- function(inv) i <<- inv ## Create the setinv function to store the calculated inverse
    
    getinv <- function() i ## Create the getinv function to retreive the cached inverse matrix
    
    list(set = set, get = get, ## Create the list of functions to be returned
         
         setinv = setinv,
         
         getinv = getinv)
    
}



## cachSolve checks if the inverse has already been calculated, if not it loads the matrix - calculates the inverse - and caches the inverse for future usage



cacheSolve <- function(x, ...) { ## the input argument is x which is the output from the makeCacheMatrix function
    
    i <- x$getinv() ## the inverse is retreived using the getinv() function
    
    if(!is.null(i)) { ## if no inverse is know, i is equal to NULL
        
        message("getting cached data") ## if i is not NULL, the inverse is known
        
        return(i) ## the cached inverse is returned and the function stops
        
    }
    
    data <- x$get() ## if i is equal to NULL the matrix is loaded using the get() function.
    
    i <- solve(data, ...) ## the inverse is calculated
    
    x$setinv(i) ## and it is cached using the setinv() function.
    
    i ## the inverse is the last objec in the function to it is returned if the function runs this far
    
}