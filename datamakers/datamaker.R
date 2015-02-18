datamaker = function(seed,args){

  set.seed(seed)
  
  library("MASS")
#meat of function
  
  disttype = args$disttype
  n = args$n
  P = args$P

  Sigmatp=function(P){
    a=array(0,dim=c(P,P))
    for(i in 1:P){
      for(j in 1:P){
        a[i,j]=max(1-0.1*(abs(i-j)),0)
      }
    }
    return(a)
  }

  
  if(disttype=="diagonal"){
    Sigma = diag(P)
    data = mvrnorm(n,rep(0,P),Sigma)
    
    Xtest = data[(n/2+1):n,]
    Xtrain = data[1:(n/2),]
    
    input = list(Xtrain = Xtrain)
    meta = list(Xtest = Xtest)
  }
  
  if(disttype=="toeplitz"){
    Sigma = Sigmatp(P)
    data = mvrnorm(n,rep(0,P),Sigma)
    
    Xtest = data[(n/2+1):n,]
    Xtrain = data[1:(n/2),]
    
    input = list(Xtrain = Xtrain)
    meta = list(Xtest = Xtest)
  }

  if(disttype=="lung1_5K"){
    data = read.table("~/mvash/mvsim/data/lung15K/lung15KVdata.txt")
    data = as.matrix(data)
    n = dim(data)[1]
    P = dim(data)[2]
  
    Xtest = data[(n/2+1):n,]
    Xtrain = data[1:(n/2),]
  
    input = list(Xtrain = Xtrain)
    meta = list(Xtest = Xtest)
  }

  #end of meat of function
  
  data = list(meta=meta,input=input)
  
  return(data)

}
