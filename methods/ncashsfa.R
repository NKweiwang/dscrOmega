ncashsfa.wrapper = function(input,args){
  
  X = input$X
  tol = args$tol
  
  library("ashr")
  # tol is tolorence
  g=try( NCashsfa(X,tol) )
  
  P=dim(X)[2]
  n=dim(X)[1]
  invpsi=try( diag(cov(g$res))^(-1) )
  invpsi=try( diag(invpsi) )
  invC=try( solve( (1/n) * ( t(g$l) %*% g$l ) ) )
  Omega=try( invpsi-invpsi%*%g$f%*%solve(invC+t(g$f)%*%invpsi%*%g$f)%*%t(g$f)%*%invpsi)
  
  return(list(Omega = Omega))    
}
