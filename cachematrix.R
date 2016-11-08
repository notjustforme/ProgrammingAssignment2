## cache the inverse of a matrix


## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function() x
        
        setinv<-function(inverse) inv<<-inverse
        
        getinv<-function() inv
        
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)

}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
}
