## The cachematrix file has two functions - makeCacheMatrix and cacheSolve.
## These functions create the inverse of a matrix and cache it, so it needn't be
## created again.

## Function makeCacheMatrix function finds the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
    xInv <- matrix()
    xCurr <- matrix()
    
    setMatrix <- function(y){
        if(!(dim(y) == dim(x)) || !all(x == y)){
            x <<- y
            xInv <<- matrix()
            message("Current Matrix changed, please calculate Inverse!!")
        }
    }
    
    setInvMatrix <- function(){
        if(!(dim(x) == dim(xCurr)) || !all(xCurr == x)){
            xInv <<- solve(x)
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
    xInv <- x$getInvMatrix()
    if(dim(xInv) == 1 && is.na(xInv[1,1])){
        x$setInvMatrix()
        xInv <- x$getInvMatrix()
        return(xInv)
    }
    message("Getting cached data")
    return(xInv)
    
}
