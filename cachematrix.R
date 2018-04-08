##  Given a matrix, makeCacheMatrix() and cacheSolve() functions calculate and cache 
##  the inverse of the matrix.
##  To test: 
##  1. first create a makeCacheMatrix object with a matrix and assign the new object to a variable, say mtx
##     m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##     mtx<-makeCacheMatrix(m1)
##  2. call cacheSolve() function 
##      cacheSolve(mtx)
##  3. return result:
##      when first time run cacheSolve(mtx), it returns the inverse of matrix m1, caches the result in mtx object
##      when continue to run cachSolve(mtx) with mtx not being reset, it returns the cacheed inverse of m1, and 
##      along with a message "getting cached inverse of matrix".
## 

##  This function creates a special "matrix" object that can cache its inverse.
##  this special "matrix" object contains 4 functions as the setters and getters 
##  for both the matrix and the inverse of the matrix
##  along with the matrix and its inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
    inverse_matrix <- NULL
    
    set <- function(mtx){
      x <<- mtx
      inverse_matrix <<- NULL
    }
    
    get <- function() { x }
    
    setInverse <- function(inverse) { inverse_matrix <<- inverse}
    
    getInverse <- function() {inverse_matrix}
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getInverse()
    
    # if the inverse is cached, return the cached
    if (!is.null(inverse_matrix)) {
        message("getting cached inverse of matrix")
        return(inverse_matrix)
    }
    
    # otherwise calculate the inverse of the matrix, and store it in x object
    # finally, return the calculated inverse of the matrix
    mtx <- x$get()
    inverse_matrix <- solve(mtx)
    x$setInverse(inverse_matrix)
    inverse_matrix
}
