tiger.wrapper = function(input,args){

  X = input$X

  out1 = sugm(X,method = "tiger")
  out1_select = sugm.select(out1, criterion = "cv")
  Omega = out1_select$opt.icov

  return(list(Omega = Omega))
}
