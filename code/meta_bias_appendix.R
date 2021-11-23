
# VEVEA AND HEDGES
# 
# 
final<-final %>% filter(!is.na(z_se))
library(weightr)

results<-weightfunct(final$z_treatment_effect, final$z_se, steps = c(0.05, 1), mods = NULL,
                     weights = NULL, fe = FALSE, table = FALSE, pval = NULL)


estimates<-toformat(results$adj_est,3)
ses<-paste("(",toformat(results$adj_se,3),")", sep='')
ses[1]<-p.stars(results$p_adj[1], ses[1])
ses[2]<-p.stars(results$p_adj[2], ses[2])

ses[3]<-p.stars(results$p_adj[3], ses[3])

vevea<-data.frame("Interval"=c("Intercept","",
                               "0.05 < p < 1",""),
                  "Adjusted Estimate"=c(estimates[2], ses[2], estimates[3], ses[3]))

names(vevea)<-c('Interval', 'Adjusted Estimate')
vevea.tex <- xtable(vevea,
                     caption="Publication Bias (Vevea and Hedges Weight Function) Adjusted Estimates",
                     align=c("llc"),format.args = list(digits = 3))

setwd(diroverleaf)


print(vevea.tex, file = "tables/vevea.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
      # sanitize.colnames.function=bold, 
      size = "small",
      caption.placement =  "top" , justify ='centre', floating=FALSE)


# p-uniform*
# 
# 
# 
puniform(yi=final$z_treatment_effect, vi=final$z_se , method = "P" , side= "right")







for (i in c('P','LNP','LN1MINP','KS')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , Kolmogorov-Smirnov 
  results.temp<-puniform(yi=final$z_treatment_effect, vi=final$z_se , method = i , side= "right") #effect sizes are in the right tail (positive)
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  
  if (i!='KS'){
    se.name<-paste('se',i, sep='')
    se.temp<-results.temp$ci.ub-results.temp$est
    se.temp<- toformat(se.temp, decimals)
    se.temp<- paste("(",se.temp,")",sep="")
    p.name<-paste('p',i, sep='')
    p.temp<-results.temp$pval.0
    assign(p.name,p.temp)
    se.temp<-p.stars(p.temp, se.temp)
    assign(se.name,se.temp)
    
  }
}


consolidated.all<- data.frame("Estimation"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "LN1MINP"=c("",coefLN1MINP,seLN1MINP,""),
                              "KS"=c("",coefKS,"-","")
)



setwd(diroverleaf)


puniform_all.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform) Adjusted Estimates")


print(puniform_all.tex, file = "tables/puniform_all.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre', floating=FALSE)



rm(coefP,seP,coefLNP,seLNP,coefLN1MINP,seLN1MINP,coefKS)


results<-puni_star(yi=final$z_treatment_effect, vi=final$z_se , method = "ML" , side= "right", alpha = 0.05)

for (i in c('P','LNP','ML')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , ML
  results.temp<-puni_star(yi=final$z_treatment_effect, vi=final$z_se , method = i , side= "right", alpha = 0.05)#effect sizes are in the right tail (positive)
  
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  
  #if (i=='ML'){
    se.name<-paste('se',i, sep='')
    se.temp<-results.temp$est-results.temp$ci.lb
    t.temp<-results.temp$est/se.temp
    
    se.temp<- toformat(se.temp, decimals)
    se.temp<- paste("(",se.temp,")",sep="")
    if(t.temp>=2.23){
      p.temp=0.001
    } else if (t.temp<2.23 & t.temp>= 1.96){
      p.temp=0.05
    } else if (t.temp < 1.96 & t.temp>=1.65){
      p.temp=0.10
    } else {
      p.temp=1
    }
    #p.name<-paste('p',i, sep='')
    #p.temp<-results.temp$pval.0
    assign(p.name,p.temp)
    se.temp<-p.stars(p.temp, se.temp)
  #}
  #else{
  #  se.temp<='---'
  #}
  assign(se.name,se.temp)
}


consolidated.all<- data.frame("Estimation"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "ML"=c("",coefML,seML,"")
)



setwd(diroverleaf)


punistar_all.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform*) Adjusted Estimates")


print(punistar_all.tex, file = "tables/punistar_all.tex", compress = FALSE, 
      include.rownames=FALSE, floating=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')


# search windows for 3 parameters of AK
lower.b = c(-Inf,0,0)
upper.b= c(Inf,Inf,1)
theta0 <- c(0.2, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig)
stepsize <- 10^(-6)
cutoffs = 1.96
symmetric = 1

LLH_only <-function (Psi) {
  A<-VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1,2)],  1),cutoffs,symmetric, final$z_treatment_effect,  final$z_se , normalizedsign = 0 );
  return (A$LLH) 
} # see if they have an R package by now (AK estimator). sets the log likelihood function given the parameters

findmin <- nlminb(objective=LLH_only, start=theta0,lower=lower.b,upper=upper.b) #takes only the Log Likelihood but not other parts of the object

results.AK <-findmin$par # the three parameters that maximize the log likelihood function. using this LL and this values to find the most likely
results.AK<-toformat(results.AK,3)
AK.se <- robust_se(findmin$par, stepsize)
AK.se<-toformat(AK.se,3)
AK.se<-paste("(", AK.se, ")***",sep='')

ak.results<-data.frame("mu"=c(results.AK[1], AK.se[1]),
                          "tau"=c(results.AK[2], AK.se[2]),
                          "beta"=c(results.AK[3], AK.se[3]))
names(ak.results)<-c("(1)", "(2)","(3)")

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\mu & \\tau & \\beta \\\\"))

ak.results.tex <- xtable(ak.results,
                           caption="Publication Bias (A-K) Adjusted Estimates")
setwd(diroverleaf)

print(ak.results.tex, file = "tables/ak.results.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      add.to.row = addtorow,
      hline.after=c(-1,0,2),
      caption.placement =  "top" , justify ='centre', floating=FALSE)

# compare with https://maxkasy.github.io/home/metastudy/ 
### AK package
final<-final %>% filter(!is.na(final$z_se))
X=as.matrix(final$z_treatment_effect)
sigma=as.matrix(final$z_se)
cluster_ID=as.matrix(final$id)

Studynames=as.character(final$archive_name)


symmetric=1
cutoffs=1.96

PublicationbiasGMM(X,sigma,cluster_ID,symmetric,cutoffs,Studynames)

test<-final %>% select(z_treatment_effect, z_se)
write.csv(test, file='aktest.csv')