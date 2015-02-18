pcsscreening  <- function(hsigma,p,delta,Kstep){
  
  allkstep=matrix(0,p,Kstep*2);
  hsigmadiag=diag(hsigma);
  for(i in 1:p){
    nonzero=i;
    value=1/hsigma[1,1];
    for(k in 2:Kstep){
      zero=setdiff(1:p,nonzero);
      eignow=eigen(hsigma[nonzero,nonzero])$values;
      if( min(abs(eignow))<delta ){ # to avoid inverting a singular matrix
        sigma11inv=solve(hsigma[nonzero,nonzero]+delta*diag(1,length(nonzero)));
      }else{
        sigma11inv=solve(hsigma[nonzero,nonzero]);
      }
      b22=diag(hsigma[zero,zero])-apply( (hsigma[zero,nonzero] %*%  sigma11inv) * hsigma[zero,nonzero],1,sum);
      b12b22inv=c(1, rep(0,length(nonzero)-1)) %*% sigma11inv %*% hsigma[nonzero,zero] /t(b22);
      cov1=sigma11inv[1,1]*rep(1,length(zero)) + b12b22inv ^2 * t(b22);
      rho=b12b22inv * sqrt(t(b22)) /sqrt(cov1);
      rho=as.vector(rho);
      maxj=which(abs(rho)==max(abs(rho)),1);
      nonzero=c(nonzero, zero[maxj]);
      value=c(value, abs(rho[maxj]));
    }
    allkstep[i,]=c(nonzero, value);
  }
  return(allkstep)
}

pcs <- function(c1,c2,delta,n,p,hsigma,allkstep,Kstep){
  library(lattice)
  library(Matrix)
  #source("pcsscreening.r")
  #allkstep = pcsscreening(hsigma,p,delta,Kstep)
  t1=c1*sqrt(2*log(p)/n);
  t2=c2*sqrt(2*log(p)/n); 
  pos=rep(0,3);
  for (i in 1:p){
    stoppos=which(allkstep[i,(Kstep+1):(2*Kstep)] < t1)[1]-1; 
    if(is.na(stoppos)){ 
      stoppos=Kstep;
    }
    nonzero=allkstep[i,1:max(stoppos,1)];
    A=hsigma[nonzero,nonzero];
    if(min(abs(eigen(A)$values))<delta){ 
      A=A+delta*diag(1,length(nonzero));
    }
    if(length(nonzero)==1){
      rowi=c(nonzero,1/A);
    }else{
      fit=solve(A)[1,];
      nonzero2=nonzero[which(abs(fit)>t2)];
      B=hsigma[nonzero2,nonzero2];
      if(min(abs(eigen(B)$values))<delta){ 
        B=B+delta*diag(1,length(nonzero2));
      }
      refit=solve(B)[1,];
      rowi=rbind(nonzero2, refit);
    }
    
    pos=rbind(pos,t(rbind(rep(i,ncol(rowi)), rowi)));
  }
  pos=pos[-1,];
  homega=sparseMatrix(i=pos[,1],j=pos[,2],x=pos[,3],dims=c(p,p));
  homega=(homega+t(homega))/2;
  
  return(homega)
}