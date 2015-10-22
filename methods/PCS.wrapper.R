
PCS.wrapper = function(input,args){
  
  
  K = args$K
  
  X = input$X
  
  n = dim(X)[1]
  P = dim(X)[2]
  
  hsigma = cov(X)
  allkstep = try(pcsscreening(hsigma,P,0.1,K) )
  Omega = try(pcs(0.2,0.2,0.1,n,P,hsigma,allkstep,K) )
  Omega = as.numeric(Omega)  
  Omega = matrix(Omega,nrow=P)
 
  return(list(Omega = Omega))
}
