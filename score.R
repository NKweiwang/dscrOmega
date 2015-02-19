#This file should define your score function

score = function(data, output){
#insert calculations here; return a named list of results
  Xtest = data$meta$Xtest
  Omega = output$Omega
  
  nt = dim(Xtest)[1]
  Pt = dim(Xtest)[2]
  
  MSE=0
  for(i in 1:nt){
    for(j in 1:Pt){
      MSE = MSE + (Xtest[i,j]-(Omega[j,j])^(-1)*(sum(Omega%*%Xtest[i,])-Omega[j,j]*Xtest[i,j]))^2
    }
  }

  
  S = cov(Xtest)
  
    f1=determinant(Omega,logarithm=TRUE)$modulus
    f2=sum(diag(S%*%Omega))
    loglik=f1-f2

  return(list(lkhd = loglik, pred = MSE ))
}
