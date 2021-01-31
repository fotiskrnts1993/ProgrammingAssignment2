## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  makeCacheMatrix is a function that creates a matrix with set function, gets its value with get function,
#  sets a value for its inverse matrix with setinverse function and gets it with getinverse.
#  Finally, we sum the results of these functions in a list. 

makeCacheMatrix <- function(x = matrix()) {
        #intializing inv as NULL
        inv<-NULL
        #define set function to assign a new value y 
        set<-function(y){
                x<<-y
                inv<<-NULL
                
        }
        
        #get function gets the value of matrix
        get<-function() (x)
        
        #define setinverse function to set the value of inverse matrix
        setinverse<-function(inverse) (inv <<- inverse)
        #getinverse gets the value of inverse matrix
        getinverse<-function() (inv)
        #get the list of the functions above
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
#  cacheSolve returns the inverse of x matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #define inv as a variable that obtains the elements of getinverse function
        inv<-x$getinverse()
        
        #if inv is not a null matrix then the cached matrix from the previous function is returned.
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        #data assigns the values of get and inv will compute the inverse of the matrix.
        #solve function computes the inverse of the x matrix, which are stored in the list of the above function
        data<-x$get()
        inv<-solve(data,...)
        #get the elements of x as assigned to the setinverse function with inv
        x$setinverse(inv)
        inv
}
