datamaker = function(args){

  library("MASS")
#meat of function
  
  disttype = args$disttype
  n = args$n
  P = args$P

  index1=sort(sample(seq(1:n),(n/2)))
  index2=seq(1:n)[-index1]

  Sigmatp=function(P){
    a=array(0,dim=c(P,P))
    for(i in 1:P){
      for(j in 1:P){
        a[i,j]=max(1-0.1*(abs(i-j)),0)
      }
    }
    return(a)
  }

  if(disttype=="lung1_5K"){
    data = read.table("~/mvash/mvsim/data/lung15K/lung15KVdata.txt")
    data = as.matrix(data)
    data = data[1:n,1:P]
    
    Xtest = data[index2,]
    Xtrain = data[index1,]
    
    input = list(Xtrain = Xtrain)
    meta = list(Xtest = Xtest)
  }
  
  if(disttype=="identity"){
    Sigma = diag(P)
    data = mvrnorm(n,rep(0,P),Sigma)
    
    Xtest = data[index2,]
    Xtrain = data[index1,]
    
    input = list(Xtrain = Xtrain)
    meta = list(Xtest = Xtest)
  }

  if(disttype=="diagonal"){
    Sigma = diag(seq(1:P)/P)
    data = mvrnorm(n,rep(0,P),Sigma)
  
    Xtest = data[index2,]
    Xtrain = data[index1,]
  
    input = list(Xtrain = Xtrain)
    meta = list(Xtest = Xtest)
  }
  


  if(disttype=="toeplitz"){
    Sigma = Sigmatp(P)
    data = mvrnorm(n,rep(0,P),Sigma)
    
    Xtest = data[index2,]
    Xtrain = data[index1,]
    
    Omega = solve(Sigma)
    
    input = list(Xtrain = Xtrain , Omega =Omega)
    meta = list(Xtest = Xtest)
  }
  #end of meat of function
  

  data = list(meta=meta,input=input)
  
  return(data)

}
