## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#1. set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inv of the matrx
#4. get the value of the inv of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinv<-function(inv) m<<-inv
    getinv<-function () m
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    if(!is.null(m)){
          message("getting cached data")
          return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinv(m)
    m
}
###Test the functions;
xx<-matrix(c(1,2,3,4,5,7,8,10,15),nrow=3,byrow=TRUE)
minv<-makeCacheMatrix(xx) 
cacheSolve(minv)
solve(xx)
