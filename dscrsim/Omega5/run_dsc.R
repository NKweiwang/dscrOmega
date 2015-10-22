library("dscr")
library("flare")
library("glasso")
library("plyr")
library("dplyr")
sourceDir("/Users/weiwang/HG/dscr_Omega/dscrOmega/datamakers")
sourceDir("/Users/weiwang/HG/dscr_Omega/dscrOmega/methods")
sourceDir("/Users/weiwang/HG/dscr_Omega/dscrOmega/score")

dscr_Omega = new.dsc("dscr_Omega","Omg")

addScenario(dscr_Omega,name="identity",datamaker,args=list(disttype="identity",n=320,P=5),seed=1:100)
addScenario(dscr_Omega,name="toeplitz",datamaker,args=list(disttype="toeplitz",n=320,P=5),seed=1:100)
# addScenario(dscr_Omega,name="lung1_5K",datamaker,args=list(disttype="lung1_5K",n=320,P=5),seed=1:20)

addMethod(dscr_Omega,name="glasso",fn =glasso.wrapper,args=list(rhostart=0.05,rhoend=0.8))
#addMethod(dscr_Omega,name="SFAmix",fn =SFAmix.wrapper,args=list(K=3))
addMethod(dscr_Omega,name="covtr",fn=covtr.wrapper,args=NULL)
addMethod(dscr_Omega,name="invcovX",fn=invcovX.wrapper,args=NULL)
addMethod(dscr_Omega,name="PCS",fn=PCS.wrapper,args=list(K=3))
addMethod(dscr_Omega,name="clime",fn=clime.wrapper,args=NULL)
addMethod(dscr_Omega,name="tiger",fn=tiger.wrapper,args=NULL)

addScore(dscr_Omega,name="likelihood",score1)
addScore(dscr_Omega,name="prediction",score2)
addScore(dscr_Omega,name="Fisher",score3)

res=run_dsc(dscr_Omega)

save(res,file="./res_5.Rdata")