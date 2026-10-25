
# Function to create a special matrix object
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    # Set the matrix and clear the cached inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # Get the matrix
    get <- function() {
        x
    }

    # Set the cached inverse
    setinverse <- function(inv) {
        inverse <<- inv
    }

    # Get the cached inverse
    getinverse <- function() {
        inverse
    }

    # Return the four functions as a list
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

# Function to calculate or retrieve the matrix inverse
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()

    # Return the cached inverse if it already exists
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # Otherwise, calculate the inverse
    data <- x$get()
    inverse <- solve(data, ...)

    # Store the inverse in the cache
    x$setinverse(inverse)

    inverse
}
