uniashsfa.wrapper = function(input,args){
  
  X = input$X
  tol = args$tol
  
  library("ashr")
  # tol is tolorence
  g= try(ashsfa(X,tol))
  
  P=dim(X)[2]
  n=dim(X)[1]
  invpsi= try ( g$sigma_e^(-2)*diag(P) )
  invC=try( solve( (1/n) * ( t(g$l) %*% g$l ) ) )
  Omega=try( invpsi-invpsi%*%g$f%*%solve(invC+t(g$f)%*%invpsi%*%g$f)%*%t(g$f)%*%invpsi )
  
  return(list(Omega = Omega))    
}
