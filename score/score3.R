score3 = function(data, output){
 
if(class(output)=='try-error'){
     Fisher = NA
 } else if(any(is.na(output$Omega))||any(output$Omega==Inf)){
        Fisher = NA
} else {
        Xtest = data$meta$Xtest
        Omega = output$Omega
	P = dim(Omega)[1]
	S = cov(Xtest)
	Fisher = sum(( (1/2)*(S %*% Omega) + (1/2)*(Omega %*% S) - diag(P))^2)
}

  return(list(Fisher = Fisher ))
}

