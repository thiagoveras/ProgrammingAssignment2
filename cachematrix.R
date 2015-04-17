## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
    
    inversed_matrix <- matrix$getInversedMatrix() # get the inversed matrix from matrix
    
    # check if is null because the first time it is setted to null
    if(!is.null(inversed_matrix)) { # if is not null...
        message("getting cached data")
        return(inversed_matrix) # return the inversed matrix
    }
    
    data <- matrix$get() # if is null... get the matrix data using matrix$get
    inversed_matrix <- solve(data) # solve the data using solve()
    matrix$setInversedMatrix(inversed_matrix) # set the inversed_matrix in matrix using matrix$setInversedMatrix
    
    return(inversed_matrix) # return the solved matrix
}
