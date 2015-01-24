#' @author  Ryan Venter <Coursera User ID 10226786>
#' @title   Programming Assignment 2: Lexical Scoping
#' @description     An example of the utilization of caching to potentially 
#'          reduce the cost of calculating the inverse of a given matrix. The 
#'          program provides no error checking and thus valid input is expected.

#-------------------------------------------------------------------------------

#' @method  makeCacheMatrix  
#' @description     An function which initializes a matrix object, x; and
#'          contains get and set helper function definitions for both x and it's
#'          inverse (inv); thus enabling the manipulation of lexical scoping 
#'          rules in order to cache (preserve) the state of x and inv.   
#' @param   x       An invertible matrix
#' @return  a list containing the defined get and set functions: set(); get();
#'          set_inverse(); get_inverse() 
#' @example y <- makeCacheMatrix(matrix(c(1,0,0,0,2,0,0,0,3), nrow = 3))
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    set_inverse <- function(inverse) {
        inv <<- inverse
    }
    
    get_inverse <- function() {
        inv
    }
    
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse) 
}

#-------------------------------------------------------------------------------

#' @method  cacheSolve
#' @description     A function which computes the inverse of a matrix, inv, of 
#'          a matrix associated with an inputted list object, x. 
#'          Should the value of inv already be cached (inv is not NULL), then 
#'          cacheSolve returns this already cached value, therefore removing the
#'          cost of having to calculate this inverse. 
#'          However, if inv is not cached, it's value will subsequently be 
#'          calcuated
#' @param   x       a list initialized by the makeCacheMatrix() function
#' @see     makeCacheMatrix()
#' @return  inv     the inverse of a matrix associated with an inputted list x
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    orig <- x$get()
    inv <- solve(orig)
    x$set_inverse(inv)
    
    inv
}