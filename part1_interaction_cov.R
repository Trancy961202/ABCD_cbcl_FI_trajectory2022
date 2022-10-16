load("/home/luoqiang/lqy/r0901/step1/demo/r0901_1.RData")


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
  results_analysis <- foreach (number=c(1:nrow(assList)), .combine=rbind) %dopar% {
    library(lmerTest)
    out <- formula(paste( assList$y.name[number], '~', paste(c(assList$cov.names.fixed[number],  assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    
    analysis <- summary(lmer.fit)[["coefficients"]]
    k<-1
    mix<-as.data.frame(matrix(NA,nrow = 1,ncol = 27))
    for(j in c(1:4,9:13)){
      mix[1,3*k-2]<-analysis[j+1,1]
      mix[1,3*k-1]<-analysis[j+1,4]
      mix[1,3*k]<-analysis[j+1,5]
      k<-k+1
    }
    result <- mix[1,]
    print(mix[1,])
  }
  for(i in 1:nrow(results_analysis)){
    rownames(results_analysis)[i]<-assList$y.name[i]
  }
  colnames(results_analysis)<-c("visit_beta","visit_t","visit_p","sex_beta","sex_t","sex_p","income1_beta","income1_t","income1_p",
                                "income2_beta","income2_t","income2_p","fam_beta","fam_t","fam_p","visit_sex_beta","visit_sex_t",
                                "visit_sex_p","visit_income1_beta","visit_income1_t","visit_income1_p",
                                "visit_income2_beta","visit_income2_t","visit_income2_p","visit_fam_beta","visit_fam_t","visit_fam_p")
  write.csv(results_analysis, outfile.name)
  cat("results are exported in a csv file and ready for download\n")
  results_analysis
}

multiAss.parallel.fit <- function (assList, data) {
  results_analysis <- foreach (number=c(1:nrow(assList))) %dopar% {
    library(lmerTest)
    out <- formula(paste( assList$y.name[number], '~', paste(c(assList$cov.names.fixed[number],  assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    lmer.fit
  }
  results_analysis
}







###  1.2 association between cbcls and covariations(interaction) #########

x.names <- ''
y.names <- colnames(data_1_1_1)[2:10]
cov.fixed <- 'visit+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+visit:sex+visit:household.income+visit:famhx_ss_momdad_hspd_p+visit:race_ethnicity'
cov.rand <- '(visit|site_id_l/rel_family_id/src_subject_id)'
# define the list for the multilevel assocaition
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
    cov.names.fixed[n] <- cov.fixed
    cov.names.rand[n] <- cov.rand
  }
}
outfile.name <- 'cbcls_interaction_cov0901.csv'
assList.cbcl.cov<- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis_cog <- multiAss.parallel(assList.cbcl.cov, data_1_1_1, outfile.name)
results_analysis_cog_fit <- multiAss.parallel.fit(assList.cbcl.cov, data_1_1_1)
stopImplicitCluster()
save.image("result0901_interaction_cov.RData")