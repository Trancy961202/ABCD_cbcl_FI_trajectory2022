load("/home/luoqiang/lqy/r0901/step1/demo/r0901_smri.RData")
load("/home/luoqiang/lqy/r0901/step3/smri/mediation/r0902_cbcl_FIfdr_smri_cor_sig.RData")


library(parallel)
library(foreach)
library(doParallel)
library(lmerTest)

detectCores()
numCores <- 6
registerDoParallel(numCores)


#####################
# definition of functions
####################


multiAss.parallel <- function (assList, data, outfile.name) {
  results_analysis <- foreach (number=c(1:nrow(assList)), .combine=rbind,.packages = "bruceR") %dopar% {
    res.med<-PROCESS(data,y=assList$y.name[number],meds=assList$z.name[number],x = assList$x.name[number],covs = c("sex","income1","income2","site02","site03","site04","site05","site06","site07","site08","site09",
                                                                                                                     "site11","site12","site13","site14","site15","site16","site17","site18","site19","site20","site21","site22",
                                                                                                                     "Black","Hispanic","Asian","Other","famhx_ss_momdad_hspd_p"),
                      ci = "bca.boot",nsim = 1000,seed = 1234)
    mix<-as.data.frame(matrix(NA,nrow = 1,ncol = 24))
    mix[1,1:8]<-res.med[["results"]][[1]][["mediation"]][1,]
    mix[1,9:16]<-res.med[["results"]][[1]][["mediation"]][2,]
    mix[1,17:24]<-res.med[["results"]][[1]][["mediation"]][3,]
    result <- mix[1,]
    print(mix[1,])
  }
  for(i in 1:nrow(results_analysis)){
    rownames(results_analysis)[i]<-paste(c(assList$x.name[i],assList$z.name[i], assList$y.name[i]), collapse=":")
  }
  write.csv(results_analysis, outfile.name)
  cat("results are exported in a csv file and ready for download\n")
  results_analysis
}







###  3.4 mediation_smri #########


# define the list for the multilevel assocaition
x.name <- rep()
y.name <- rep()
z.name <- rep()
n <- 0
for (i in 1:nrow(cbcl_FIfdr_smri_cor_sig)){
  n <- n + 1
  x.name[n] <- cbcl_FIfdr_smri_cor_sig[i,]$smri
  y.name[n] <- cbcl_FIfdr_smri_cor_sig[i,]$cbcl
  z.name[n] <- cbcl_FIfdr_smri_cor_sig[i,]$FI
}
outfile.name <- 'mediation0901.csv'
assList.cbcl.sex.smri<- data.frame(x.name, y.name, z.name)
results_analysis_cog <- multiAss.parallel(assList.cbcl.sex.smri, df, outfile.name)
stopImplicitCluster()
save.image("result0901_mediation_smri.RData")