## Programming Assignment 2 (Week 3) : R Programming Course

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inversed matrix
# get the value of the inversed matrix
makeCacheMatrix <- function(matrix = matrix()) {
    
    inversed_matrix <- NULL 
    
    set <- function(input_matrix) {
        matrix <<- input_matrix # initialises matrix to input_matrix
        inversed_matrix <<- NULL # initialises inversed matrix to null
    }
    
    get <- function() {
        return(matrix) # get the input matrix
    }
    
    setInversedMatrix <- function(input_inversed_matrix) {
        inversed_matrix <<- input_inversed_matrix # set the inversed matrix
    }
    
    getInversedMatrix <- function() {
        return(inversed_matrix) #get the inversed matrix
    }
    
    list(set = set, get = get,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}


## The second function calculates the inversed matrix of the special "matrix" created with the above function. 
# However, it first checks to see if the inversed matrix has already been calculated. 
# If so, it gets the inversed matrix from the cache and skips the computation. 
# Otherwise, it calculates the inversed matrix of the data and sets the value of the inversed matrix in the cache.
cacheSolve <- function(matrix, ...) {
    
    inversed_matrix <- matrix$getInversedMatrix() # get the inversed matrix from matrix
    
    # check if the inversed matrix has already been calculated
    if(!is.null(inversed_matrix)) { # if yes...
        message("getting cached data")
        return(inversed_matrix) # return the inversed matrix of the cache skips the computation
    }
    
    data <- matrix$get() # if no... get the matrix data using matrix$get
    inversed_matrix <- solve(data) # compute the inversed matrix of the data using solve()
    matrix$setInversedMatrix(inversed_matrix) # set the inversed_matrix in the cache using matrix$setInversedMatrix
    
    return(inversed_matrix) # return the inversed matrix
}
