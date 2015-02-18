SAFmix.wrapper = function(input,args){
  
  X = input$X
  K = args$K
  
  write.table(X,file="dscrsfamix.txt")
  
  system("mkdir SFAmixout")
  
  # system("~/mvash/mvsim/SFAmix/SFAmix --nf K --y ./dscrsfamix.txt --out ./SFAmixout --sep space")
  
  system.call(paste("~/mvash/mvsim/SFAmix/SFAmix","--nf",K,"--y","./dscrsfamix.txt","--out",
                    "./SFAmixout","--sep","space",sep=" "))
  
  Fhat=read.table("./SFAmixout/EX")
  lambda=read.table("./SFAmixout/LAM")
  alpha=read.table("./SFAmixout/PSI")
  
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
  
  
  return(list(Omega = Omega))    
}
