load("/home/luoqiang/lqy/r0901/step1/demo/inter_slope_dmri0902.RData")


library(parallel)
library(foreach)
library(doParallel)
library(lmerTest)

detectCores()
numCores <- 8
registerDoParallel(numCores)


#####################
# definition of functions
####################


multiAss.parallel <- function (assList, data, outfile.name) {
  results_analysis <- foreach (number=c(1:nrow(assList)), .combine=rbind,.packages = "lmerTest") %dopar% {
    out <- formula(paste(assList$y.name[number], '~', paste(c(assList$x.name[number], assList$cov.names.fixed[number], assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    
    mix<-as.data.frame(matrix(NA,nrow = 1,ncol = 3))
    mix[1,]<- summary(lmer.fit)[["coefficients"]][2,c(1,4,5)]
    result <- mix[1,]
    print(number)
    print(result)
  }
  for(i in 1:nrow(results_analysis)){
    rownames(results_analysis)[i]<-paste(c(assList$x.name[i], assList$y.name[i]), collapse=":")
  }
  colnames(results_analysis)<-c("cog_beta","cog_t","cog_p")
  write.csv(results_analysis, outfile.name)
  cat("results are exported in a csv file and ready for download\n")
  results_analysis
}







###  3.5 association between cbcl and dmri #########

x.names <- colnames(data_com_dmri_tbss)[33:1217]
y.names <- colnames(data_com_dmri_tbss)[2:19]
cov.fixed <- 'sex+income1+income2+site02+site03+site04+site05+site06+site07+site08+
site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
site21+site22+famhx_ss_momdad_hspd_p'
cov.rand <- '(1|site_id_l/rel_family_id)'
# define the list for the multilevel assocaition
x.name <- rep()
y.name <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
for (i in x.names){
  for(j in y.names){
    n <- n + 1
    x.name[n] <-  i
    y.name[n] <- j
    cov.names.fixed[n] <- cov.fixed
    cov.names.rand[n] <- cov.rand
  }
}
outfile.name <- 'cbcls_dmri_correlation0902.csv'
assList.cbcl.cog.dmri<- data.frame(x.name, y.name,cov.names.fixed,cov.names.rand)
results_analysis_cog <- multiAss.parallel(assList.cbcl.cog.dmri, data_com_dmri_tbss, outfile.name)

stopImplicitCluster()
save.image("result0902_cbcls_dmri_correlation.RData")