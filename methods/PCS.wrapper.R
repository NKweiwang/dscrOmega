
PCS.wrapper = function(input,args){
  
  
  K = args$K
  
  X = input$X
  
  n = dim(X)[1]
  P = dim(X)[2]
  
  hsigma = cov(X)
  allkstep = pcsscreening(hsigma,P,0.1,K)
  Omega=pcs(0.2,0.2,0.1,n,P,hsigma,allkstep,K)
  
  return(list(Omega = Omega))
}
