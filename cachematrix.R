## First function creates a template for object that caches a matrix and it's inverse
## and 'methods' to deal with them (read and write them)
## Second function checks whether we need to calculate new inverse or not (which is illogical
## to me, at least yet, I've created a post at the forum on the matter)

## Cache for matrix, it's inverse, plus four methods (read/write matrix/it's inverse)
## There's also a flag indicating whether a new matrix was set (so we need to re-calculate 
## the inverse)

MakeCacheMatrix<-function(CurMatrix=matrix()) {
  FlagInvMatrixCalculated<-FALSE
  InvMatrix=matrix()
  set_matrix<-function(NewMtx)
  {
    CurMatrix<<-NewMtx
    FlagInvMatrixCalculated<<-FALSE
  }
  set_inverse<-function(InvCalc)
  {
    InvMatrix<<-InvCalc
    FlagInvMatrixCalculated<<-TRUE
  }
  get_cur_matrix<-function() CurMatrix
  get_inverse<-function() InvMatrix
  
  list(set_matrix=set_matrix, set_inverse=set_inverse,
       get_cur_matrix=get_cur_matrix, get_inverse=get_inverse, flag=FlagInvMatrixCalculated)
}

## Here we instantiate a new 'MakeCacheMatrix' object, named MatrixList

MatrixList<-MakeCacheMatrix(matrix(1:4,2,2))


## We check whether it's a new matrix or not, and if it's new, we calculate (and write to cache)
## new inverse, otherwise we read inverse from cache

cacheSolve <- function(MatrixList) {
        ## Return a matrix that is the inverse of 'x'
  if(!MatrixList$flag) ## we need to calculate the inverse
  { 
    NewInverse<-solve(MatrixList$get_cur_matrix())
    MatrixList$set_inverse(NewInverse)
  }  else  {
    message("Getting cached data")
    NewInverse<-MatrixList$get_inverse()
  }
  NewInverse
}