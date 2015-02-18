sourceDir("datamakers")
scenarios=list()

#Now, for each scenario create an element of scenarios of the following form
#scenarios[[1]]=list(name="",fn=,args,seed=1:100)
scenarios[[1]]=list(name="diagonal",fn=datamaker,args=list(disttype="diagonal",n=20,P=50),seed=1:2)
scenarios[[2]]=list(name="toeplitz",fn=datamaker,args=list(disttype="toeplitz",n=20,P=50),seed=1:2)
#scenarios[[3]]=list(name="lung1_5K",fn=datamaker,args=list(disttype="Cauchy",nsamp=1000),seed=1:100)

