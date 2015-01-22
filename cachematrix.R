###############################################################################
##
##	Project:	Programming Assignment 2: Lexical Scoping
##	File:		cachematrix.R
##	Location:	https://github.com/ochuzel/ProgrammingAssignment2
##	Description: 
##  This file implements implement a cache 
##  to save time consuming computations of matrix inversion.
##  
##  makeCacheMatrix and cacheSolve implement the cache. 
##  makeCacheMatrix implements the setters and getters to access both the input  
##  matrix and its inverse
##  whereas cacheSolve implements the cache and uses the setters and getters  
##  defined in makeCacheMatrix.
##  The main idea is to implement the encapsulation of the input and result 
##  matrixes using the superassignment operator "<<-" 
##  in conjunction with lexical scoping.
##  The use of <<- causes a search to made through parent environments for an 
##  existing definition of the variable being assigned.
##
## Example#1 of use
## qa<-makeCacheMatrix()
## m <- matrix(1:4, nrow = 2, ncol = 2)
## qa$set(m)
## cacheSolve(qa)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Example#2 of use
## qap<-makeCacheMatrix(matrix(4:1, nrow = 2, ncol = 2))
## cacheSolve(qap)
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
##
##
##	Created:	22 January 2015
##	Author:		
##	Status:		
##
###############################################################################

## This function provides the setters and getters for 
## both the matrix and its inverse.
## It uses the superassignment operator "<<-" in conjunction with 
## lexical scoping.
## In both setters, the environment stores state for the set and 
## setinv functions that modify the state by using superassignment. 
## Thus, each input matrix and its inverse matrix are in the same encapsulated 
## environment.
## Each cached inverse matrix cannot be mixed up with another cached inverse 
## matrix since the superassignment operator is not used for 
## the assignation in the global environment
##
makeCacheMatrix <- function(x = matrix) {        
        ## Cached matrix is null whether or not the set function is called
        ## Thus, it still works if the matrix is provided (optional) via 
        ## the argument x of makeCacheMatrix and set is not called
        m <- NULL
        ## The setter of the input matrix
        set <- function(y) {
                ## No check whether or not the matrix is invertible
                ## Use of superassignment in order to manipulate x and m in 
                ## the same environment
                x <<- y
                ## Cached invers matrix is bound to NULL
                ## The inverse matrix will have to be recomputed in cacheSolve
                ## Use of superassignment to target the m defined in 
                ## the parent environment
                m <<- NULL
        }
        ## The getter of the input matrix
        get <- function() x

        ## The setter of the inverse matrix
        ## Use of superassignment for assigning the m defined in 
        ## the parent environment
        setinv <- function(mtx) m <<- mtx
        ## The getter of the inverse matrix
        getinv <- function() m

        ## Returned value as the list of the previous functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function implements the cache using the following strategy:
## It returns the inverse matrix:
## either by providing the cached matrix if the matrix has not changed 
## since the previous call (if any)
## or by computing the inverse matrix if the cached matrix was not available
## (in this case it is the first call)
## or by recomputing the inverse matrix if the matrix has changed 
## It uses the accessors defined in makeCacheMatrix and associated to x 
## passed as argument.
##
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        ## Test whether the matrix has changed 
        ## I.e when set was called m was bound to NULL
        if(!is.null(m)) {
                ## Cached inverse matrix is returned if the matrix has not 
                ## changed
                message("getting cached data")
                return(m)
        }
        ## First time the cacheSolve is called for that matrix
        ## or the matrix has changed, 
        ## The inverse matrix is recomputed and cached 
        ##  
        ## Retrieval of the matrix using get
        data <- x$get()
        ## Computing the inverse of the matrix
        m <- solve(data, ...)
        ## Setting the result in x
        x$setinv(m)

        ## The matrix that is the inverse of 'x' is returned
        m
}

