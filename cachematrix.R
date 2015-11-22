## makeCacheMatrix and cacheSolve calculate the inverse of a matrix. These functions make use of the cache, which can save time if the matrix is very large and the inverse needs to be calculated repeatedly, provided that the original matrix does not change.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse by storing a list of four functions (set, get, setmean, getmean)

makeCacheMatrix <- function(x = matrix()) {
        # Define variable i and set initially as NULL
        i <- NULL
        
        # Define the set function, which holds variables x and i, as y and NULL, respectively 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # Define the get function, which returns the vector x stored in the main function
        get <- function() x
        
        # Define the setinverse function, which holds the value of the inverse in variable i into the main function         
        setinverse <- function(inverse) i <<- inverse
        
        # Define the getinverse function, which returns the value of i
        getinverse <- function() i
        
        # Return a list with each of the functions defined
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first checks to see if the inverse of the matrix from makeCacheMatrix has already been calculated and stored in the cache; if so, and if the matrix has not changed, cacheSolve returns this cached value. Otherwise, it calculates the inverse of the special "matrix" returned by makeCachematrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Store the inverse matrix from makeCacheMatrix as i, using the get function to return the vector x stored in the main function
        i <- x$getinverse()
        
        # If i has a value (e.g., is already cached), return cached value with accompanying message.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # Otherwise, return the inverse matrix from makeCacheMatrix and store as variable data.
        data <- x$get()
        
        # Calculate inverse of x using solve function 
        i <- solve(data, ...)
        
        # Store the inverse of x as variable i
        x$setinverse(i)
        
        # Return value of variable i to user
        i
}
