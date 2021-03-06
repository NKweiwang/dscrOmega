library("dscr")
library("BatchExperiments")
source("scenarios.R")
source("methods.R")
source("score.R")
reset_dsc(scenarios,methods, force=TRUE)

res=run_dsc(scenarios,methods,score)
dsc = list(scenarios=scenarios,methods=methods,scorefn=score)
system("rm -r one_sample_location-files")

reg=dsc2BE(dsc,"one_sample_location")
summarizeExperiments(reg)

id1 <- findExperiments(reg, algo.pattern="mean")[1]
testJob(reg,id1)


chunked <- chunk(getJobIds(reg), n.chunks = 10, shuffle = TRUE)
timetaken=system.time(submitJobs(reg, chunked))

res2=reduceResultsExperiments(reg, ids=findDone(reg))

aggregate(squared_error~algo+prob,data=res2,mean)
aggregate(squared_error~method+scenario,res,mean)

aggregate(abs_error~algo+prob,data=res2,mean)
aggregate(abs_error~method+scenario,res,mean)