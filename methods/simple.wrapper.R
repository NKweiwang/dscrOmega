covtr.wrapper = function(input,args){
  
  X = input$X
  
  Omega = diag( (diag( cov(X) ))^(-1) )
  return(list(Omega = Omega))
}
