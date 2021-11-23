
dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/"
setwd(dir)

source("code/aux/PublicationbiasGMM/R/PublicationbiasPackage.R")
source("code/aux/AndrewsKasyRCode/VariationVarianceLogLikelihood.R") # Andrews & Casy (2017) selection model
source("code/aux/AndrewsKasyRCode/AuxiliaryFunctions.R") #for calculating se of beta estimate
source("code/aux/AndrewsKasyRCode/Step_function_normal_cdf.R")
source("code/aux/AndrewsKasyRCode/ReplicationAnalyticLogLikelihood.R")

load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')

a<-final %>% filter(authors=='Kessler et al.')
b<-final_one %>% filter(id=='Leventhal & Dupere 2011')

akcsv<-final %>% filter(!is.na(z_se)) %>% select(z_treatment_effect, z_se )
setwd('/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data/raw')
write.csv(akcsv, file='ackcsv2.csv',col.names = FALSE, row.names=FALSE)


### AK package

# 
# final<-final %>% filter(!is.na(final$z_se))
# X=as.matrix(final$z_treatment_effect)
# sigma=as.matrix(final$z_se)
# cluster_ID=as.matrix(final$id)
# 
# Studynames=as.character(final$archive_name)
# 
# symmetric=1
# cutoffs=1.96
# here::i_am('./code/aux/PublicationbiasGMM/R/PublicationbiasPackage.R')
# here::here()
# 
# PublicationbiasGMM(X,sigma,cluster_ID,symmetric,cutoffs,Studynames)
# 
# Psihat0 <<- matrix(1,1,length(cutoffs)); #betap
# Psihat0_theta <<- matrix(0:1,1,2); #[theta, sd(theta)]
# 
# #Primitives
# n=length(X);
# C=matrix(1,n,1);
# name='GMMResults';
# 
# includeinfigure <<- array(matrix(1,n,1));
# includeinestimation <<- array(matrix(1,n,1));
# 
# #If you want to have the figues that specify where the estimates fall with
# #respect to significance in a 5% level
# dofigures <<- 1;
# 
# #If you want to get the step function p values at the points of discontinuity
# doestimates <<- 1;
# 
# #If you want to get robust confidence sets
# dorobust <<- 1;
# 
# #If you want the corrections and figures that accompany the corrections
# docorrections=1;
# 
# # Producing figures -------------------------------------------------------
# if (dofigures==1) {
#   DescriptiveStats(X,sigma)
# }
# 
# 
# # Estimating the model ----------------------------------------------------
# if (doestimates==1) {
#   Estimates=EstimatingSelection(X,sigma,symmetric,cluster_ID,cutoffs,Studynames)
#   Psihat=Estimates$Psihat
#   Varhat=Estimates$Varhat
# }

### manually 

# search windows for 3 parameters of AK
lower.b = c(-Inf,0,0)
upper.b= c(Inf,Inf,Inf)
theta0 <- c(0.11, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig). Review if these are reasonable values 
stepsize <- 10^(-6)
cutoffs = c(1.96)
symmetric = 1


data_temp<-final %>% select(z_treatment_effect, z_se) %>% filter(!is.na(z_se))%>% filter(!is.na(z_treatment_effect))

LLH_only <-function (Psi) {
  A<-VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1,2)],  1),cutoffs,symmetric, data_temp$z_treatment_effect,  data_temp$z_se , normalizedsign = 0 );
  return (A$LLH) 
} # see if they have an R package by now (AK estimator). sets the log likelihood function given the parameters

findmin <- nlminb(objective=LLH_only, start=theta0,lower=lower.b,upper=upper.b) #takes only the Log Likelihood but not other parts of the object

estimation <-findmin$par # the three parameters that maximize the log likelihood function. using this LL and this values to find the most likely
se <- robust_se(findmin$par, stepsize)

ak<-rbind(estimation, se)
ak<-as.data.frame(ak)
names(ak)<-c('mu','tau','beta')

setwd(diroverleaf)


ak.tex <- xtable(ak, caption="Publication Bias AK Estimates")


print(ak.tex, file = "tables/ak.tex", compress = FALSE, 
      include.rownames=TRUE,
      size = "small",
      caption.placement =  "top" , justify ='centre')


# make an appendix table to make the tay, beta, andd efefcta size smaller, and bigger

