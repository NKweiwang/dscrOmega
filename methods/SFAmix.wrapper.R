SFAmix.wrapper = function(input,args){
  
  X = input$X
  K = args$K
  
  write.table(X,file="dscrsfamix.txt",row.names=F,col.names=F)
  print(class(X))
  print(dim(X))

  system("mkdir SFAmixout")
  
  # ~/mvash/mvsim/SFAmix/SFAmix --nf K --y dscrsfamix.txt --out SFAmixout --sep space
  system(paste("/Users/weiwang/HG/dscr_Omega/dscrOmega/SFAmix/SFAmix","--nf",K,"--y","dscrsfamix.txt","--out",
               "SFAmixout","--sep","space",sep=" "))
  
  alpha=read.table("./SFAmixout/PSI")
  
  if(file.info("./SFAmixout/EX")$size == 1){
    Psi=as.vector(alpha)
    invpsi=Psi^(-1)
    invpsi=as.vector(invpsi)
    invpsi=diag(invpsi)
    Omega=invpsi
  } else{
    Fhat=read.table("./SFAmixout/EX")
    lambda=read.table("./SFAmixout/LAM")
    Psi=as.vector(alpha)
    Fhat=as.matrix(Fhat)
    lambda=as.matrix(lambda)
    invpsi=Psi^(-1)
    invpsi=as.vector(invpsi)
    invpsi=diag(invpsi)
    P=dim(lambda)[2]
    n=dim(Fhat)[1]
    invC=solve( (1/n) * ( t(Fhat) %*% Fhat ) )
    
    Omega=invpsi-invpsi%*%t(lambda)%*%solve(invC+lambda%*%invpsi%*%t(lambda))%*%lambda%*%invpsi
  }
  
  return(list(Omega = Omega))    
}
