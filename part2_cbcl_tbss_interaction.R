load("/home/luoqiang/lqy/r0901/step1/demo/r0901_tbss.RData")

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
  results_analysis <- foreach (number=c(1:nrow(assList)), .combine=rbind,.packages="lmerTest") %dopar% {
    out <- formula(paste( assList$y.name[number], '~', paste(c(assList$x.name[number], assList$cov.names.fixed[number], assList$inter[number], assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    
    analysis <- summary(lmer.fit)[["coefficients"]]
    k<-1
    mix<-as.data.frame(matrix(NA,nrow = 1,ncol = 33))
    for(j in c(1:5,10:14,19)){
      mix[1,3*k-2]<-analysis[j+1,1]
      mix[1,3*k-1]<-analysis[j+1,4]
      mix[1,3*k]<-analysis[j+1,5]
      k<-k+1
    }
    result <- mix[1,]
    print(mix[1,])
  }
  for(i in 1:nrow(results_analysis)){
    rownames(results_analysis)[i]<-paste(c(assList$x.name[i], assList$y.name[i]), collapse=":")
  }
  colnames(results_analysis)<-c("cog_beta","cog_t","cog_p","visit_beta","visit_t","visit_p","sex_beta","sex_t","sex_p","income1_beta","income1_t","income1_p",
                                "income2_beta","income2_t","income2_p","fam_beta","fam_t","fam_p","visit_sex_beta","visit_sex_t","visit_sex_p",
                                "visit_income1_beta","visit_income1_t","visit_income1_p","visit_income2_beta","visit_income2_t","visit_income2_p",
                                "visit_fam_beta","visit_fam_t","visit_fam_p","visit_cog_beta","visit_cog_t","visit_cog_p")
  write.csv(results_analysis, outfile.name)
  cat("results are exported in a csv file and ready for download\n")
  results_analysis
}







###  2.1 association between cbcl and tbss #########

x.names <- colnames(data_cbcls_tbss)[37:46]
y.names <- colnames(data_cbcls_tbss)[2:10]
cov.fixed <- 'visit+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+visit:sex+visit:household.income+visit:famhx_ss_momdad_hspd_p+visit:race_ethnicity'
cov.rand <- '(visit|site_id_l/rel_family_id/src_subject_id)'
#cov.rand <- '(visit|src_subject_id)'
# define the list for the multilevel association
x.name <- rep()
y.name <- rep()
inter <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
for (i in x.names){
  for(j in y.names){
    n <- n + 1
    x.name[n] <-  i
    y.name[n] <- j
    inter[n] <- paste(i,"visit",sep = ":")
    cov.names.fixed[n] <- cov.fixed
    cov.names.rand[n] <- cov.rand
  }
}
outfile.name <- 'interaction_cbcls_tbss0903.csv'
assList.cbcl.tbss<- data.frame(x.name, y.name, cov.names.fixed, inter,cov.names.rand)
results_analysis_cog <- multiAss.parallel(assList.cbcl.tbss, data_cbcls_tbss, outfile.name)
stopImplicitCluster()
save.image("result0901_cbcl_tbss_interaction.RData")