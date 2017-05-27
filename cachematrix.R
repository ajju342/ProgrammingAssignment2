## The cachematrix file has two functions - makeCacheMatrix and cacheSolve.
## These functions create the inverse of a matrix and cache it, so it needn't be
## created again.

## Function makeCacheMatrix function finds the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize empty matrices
    ## Inverse matrix
    xInv <- matrix()
    ## Matrix pertaining to the inverse matrix
    xCurr <- matrix()
    
    setMatrix <- function(y){
        ## Compare new matrix to the current matrix
        if(!(dim(y) == dim(x)) || !all(x == y)){
            ## Assign the new matrix
            x <<- y
            ##Empty the Inverse matrix as new matrix is different from original
            xInv <<- matrix()
            ##Prompt a message to calculate the inverse as the matrix changed
            message("Current Matrix changed, please calculate Inverse!!")
        }
    }
    
    setInvMatrix <- function(){
        ##Compare the current matrix with matrix related to the inverse matrix 
        if(!(dim(x) == dim(xCurr)) || !all(xCurr == x)){
            ## Calculate the inverse
            xInv <<- solve(x)
            ## As the new matrix is different, modify this placeholder matrix
            xCurr <<- x
        }
    }
    
    getMatrix <- function() x
    
    getInvMatrix <- function() xInv

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Function cacheSolve caches the inverse matrix. And in case the matrix is not
## modified, it returns the cached inverse matrix. However, if the matrix is
## modified, it re-created the inverse and caches it again.

cacheSolve <- function(x, ...) {
    
    ## Get Inverse matrix
    xInv <- x$getInvMatrix()
    
    ## Check if Inverse is empty
    if(dim(xInv) == 1 && is.na(xInv[1,1])){
        ## If empty calculate the new inverse matrix
        x$setInvMatrix()
        xInv <- x$getInvMatrix()
        return(xInv)
    }
    
    ## Retrieve the stored inverse matrix, as it is not empty!
    message("Getting cached data")
    return(xInv)
    
}
