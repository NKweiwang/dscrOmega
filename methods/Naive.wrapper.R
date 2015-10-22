invcovX.wrapper = function(input,args){
  
  X = input$X
  
  Omega = solve(cov(X))
  return(list(Omega = Omega))
}
