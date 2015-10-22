#' @title fit Y=lf+e for given l 
#' @param Y an n by p matrix
#' @param l an n-vector
est_f = function(Y,l,ash.maxit=5000){  
  betahat=(sum(l^2))^(-1)*(t(l)%*%Y) 
  betahat=as.vector(betahat)
  sigmae2 = mean(Y^2)
  sebeta=sqrt(sigmae2/(sum(l^2)))
  mixsd = c(exp(seq(log(0.01),log(2),length=10)))
  ash.beta=ash(betahat,sebeta,method="fdr",mixsd=mixsd,control=list(maxiter=0),mixcompdist="normal")
  return(ash.beta$PosteriorMean)
  # return(ash(betahat,sebeta,method="fdr",df=dim(Y)[1],mixsd=mixsd,maxiter=ash.maxit)$PosteriorMean)
}

est_l = function(Y,f,ash.maxit=1000){
  betahat=(sum(f^2))^(-1)*(t(f)%*%t(Y)) 
  betahat=as.vector(betahat)
  sigmae2 = mean(Y^2)
  sebeta=sqrt(sigmae2/(sum(f^2)))
  mixsd = c(0,exp(seq(log(0.01),log(2),length=10)))
  # return(ash(betahat,sebeta,method="fdr",df=dim(Y)[2],mixsd=mixsd,maxiter=ash.maxit)$PosteriorMean)
  return(ash(betahat,sebeta,method="fdr",mixsd=mixsd,control=list(maxiter=0),mixcompdist="normal")$PosteriorMean)
}

#' @title fits Y=lf+e for rank one
#' @param Y an n by p matrix
#' @return list with elements l an n-vector, f a p-vector and Yres=Y-lf'
greedy = function(Y,maxit=100){
  #initialize l with the first PC in this direction by SVD
  l = svd(Y)$u[,1]
  prefl=0
  # set stop criterion
  stopest=1
  while(abs(stopest)>1e-6){
    f=est_f(Y,l)
    l=est_l(Y,f)
    stopest=mean((l%*%t(f))^2)-prefl
    prefl=mean((l%*%t(f))^2)
    #print(stopest)
  }
  Yres = Y-l%*%t(f)
  return(list(Yres=Yres,Loading=l,Factor=f,sigmae=sqrt(mean(Yres^2))))
}

# this is ashsfa just for constant variance

ashsfa=function(Y,tol){
  g1=greedy(Y)
  res=g1$Yres
  l=g1$Loading
  f=g1$Factor
  Lout=l
  Fout=f
  if(mean((g1$Loading%*%t(g1$Factor))^2)/mean(Y^2)<tol){
    return(list(l=l,f=f,res=res,sigma_e=sqrt(mean(res^2))))
  } else {
    tau=1
    while(mean((l%*%t(f))^2)/mean(Y^2)>=tol){
      g2=greedy(res)
      res=g2$Yres
      l=g2$Loading
      f=g2$Factor
      Lout=cbind(Lout,l)
      Fout=cbind(Fout,f)
      tau=tau+1
      print(tau)
    }
    return(list(l=Lout,f=Fout,res=res,sigma_e=sqrt(mean(res^2))))
  }
}
