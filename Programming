makeCacheMatrix <- function(a=matrix(),...){
       m<-NULL
       set<-function(b){
               a << - b
               m<<-NULL
       }
       get<-function() a
       setM<-function(solve) m<<-solve
       getM<-function() m
       lst(set=set,get=get,setM=setM,getM=getM)
 }
 
 
 #this computes the inverse of the matrix by makeCacheMatrix
 cacheSolve <- function(a, ...) {
   m <- a$getM()
   if(!is.null(m)){
   message("get cached data")
   return(m)
   }
   data <- a$get()
   m <- solve(data,...)
   a$setM(m)
   m
   ## Return matrix inverse of 'a'
 }
