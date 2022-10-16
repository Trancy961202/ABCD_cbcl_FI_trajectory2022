load("/home/luoqiang/lqy/r0901/step1/demo/inter_slope0901.RData")

library(parallel)
library(foreach)
library(doParallel)
library(lmerTest)

detectCores()
numCores <- 16
registerDoParallel(numCores)

multiAss.parallel <- function (assList, data, outfile.name) {
  results_analysis <- foreach (number=c(1:nrow(assList)), .combine=rbind,.packages = "lmerTest") %dopar% {
    out <- formula(paste(assList$y.name[number], '~', paste(c(assList$x.name[number],'sex', assList$cov.names.fixed[number],assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    
    mix<-as.data.frame(matrix(NA,nrow = 1,ncol = 3))
    mix[1,1:3]<- summary(lmer.fit)[["coefficients"]][2,c(1,4,5)]
    result <- mix[1,]
    print(number)
    print(result)
  }
  for(i in 1:nrow(results_analysis)){
    rownames(results_analysis)[i]<-c(assList$x.name[i])
  }
  colnames(results_analysis)<-c("cog_beta_fi","cog_t_fi","cog_p_fi")
  write.csv(results_analysis, outfile.name)
  cat("results are exported in a csv file and ready for download\n")
  results_analysis
}







###  3.3 association between FI and smri #########

x.names <- colnames(data_com_smri_tbss)[c(num_smri_total,num_thick,num_area,num_vol)]
y.names <- 'nihtbx_fluidcomp_fc'
cov.fixed <- 'income1+income2+site02+site03+site04+site05+site06+site07+site08+
site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
site21+site22+famhx_ss_momdad_hspd_p+smri_vol_scs_intracranialv'
cov.rand <- '(1|site_id_l/rel_family_id)'
# define the list for the multilevel association
x.name <- rep()
y.name <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
for (i in x.names){
  n <- n + 1
  x.name[n] <-  i
  y.name[n] <- y.names
  cov.names.fixed[n] <- cov.fixed
  cov.names.rand[n] <- cov.rand
}
outfile.name <- 'FI_smri_correlation.csv'
assList.cbcl.cog.smri<- data.frame(x.name, y.name,cov.names.fixed,cov.names.rand)
results_analysis_smri <- multiAss.parallel(assList.cbcl.cog.smri, data_com_smri_tbss, outfile.name)

stopImplicitCluster()
save.image("result0901_FI_smri_correlation.RData")

