glasso.wrapper = function(input,args){
  
  rhostart = args$rhostart
  rhoend = args$rhoend
  
  library("glasso")
  X = input$X
  
  # do 10 fold CV for glasso
  optrho=0
  library("permute")
  index=shuffle(dim(X)[1])
  K=20
  rholist=seq(rhostart,rhoend,length.out=K)
  tempscore=-Inf
  for(k in 1:K){
    rho=rholist[k]
    likscore=0
    for(i in 1:10){
      index1=index[(1+(i-1)*(dim(X)[1])/10):(i*(dim(X)[1])/10)]
      covX = cov(X[-index1,])
      Omega = glasso(covX,rho=rho)$wi
      S = cov(X[index1,])
      f1=determinant(Omega,logarithm=TRUE)$modulus
      f2=sum(diag(S%*%Omega))
      loglik=f1-f2
      likscore=likscore+loglik
    }
    
    if(likscore>tempscore){
      optrho=rho
      tempscore=likscore
      print(tempscore)
    }
  }
  
  covX = cov(X)
  Omega =try( glasso(covX,rho=optrho)$wi )
  
  return(list(Omega = Omega))  
}
