glasso.wrapper = function(input,args){
  
  rho = args$rho
  
  library("glasso")
  X = input$X
  covX = cov(X)
  Omega = glasso(covX,rho=rho)$wi
  
  return(list(Omega = Omega))  
}
