#This file should define your score function

score1 = function(data, output){
#insert calculations here; return a named list of results
 if(class(output)=='try-error'){
     loglik = NA
 } else if(any(is.na(output$Omega))||any(output$Omega==Inf)){
	loglik = NA
} else {
	Xtest = data$meta$Xtest
	Omega = output$Omega
	S = cov(Xtest)
	evlsig=try( eigen(Omega)$values)
	f1=try(sum(log(evlsig)))
	f2=try(sum(diag(S%*%Omega)))
	loglik=f1-f2
}

  return(list(lkhd = loglik ))
}
