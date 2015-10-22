#This file should define your score function

score2 = function(data, output){
  #insert calculations here; return a named list of results
  if(class(output)=='try-error'){
     MSE = NA
 } else if(any(is.na(output$Omega))||any(output$Omega==Inf)){
        MSE = NA
} else {
        Xtest = data$meta$Xtest
        Omega = output$Omega
	nt = dim(Xtest)[1]
	Pt = dim(Xtest)[2]
	MSE=0
        for(i in 1:nt){
           for(j in 1:Pt){
                MSE = MSE + ( Xtest[i,j] + (Omega[j,j])^(-1)*(sum(Omega[,j]*Xtest[i,])-Omega[j,j]*Xtest[i,j]) )^2
           }
        }
}
  
  return(list( pred = MSE ))
}
