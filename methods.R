sourceDir("methods")

methods=list()
#now for each method define a list with its name, function and arguments (if no additional arguments use NULL)
# like this: 
#methods[[1]] = list(name="methodname",fn = function,args=NULL)
methods[[1]] = list(name="glasso",fn =glasso.wrapper,args=list(rho=0.5))
  methods[[2]] = list(name="PCS",fn=PCS.wrapper,args=list(K=20))
  #methods[[3]] = list(name="SFAmix",fn=SFAmix.wrapper,args=NULL)
 
