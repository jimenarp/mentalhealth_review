setwd(dir)

load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')

decimals<-3


final<-final %>% filter(!is.na(z_se))
final_one<-final_one %>% filter(!is.na(one_outcome_sd))

n<-nrow(final)
n1<-nrow(final_one)

### PUNIFORM #####

# can be for P, LNP or ML ( Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , ML)

results.p<-puni_star(yi=final$z_treatment_effect, vi=final$z_se , method ='P' , side= "right", alpha = 0.05)#effect sizes are in the right tail (positive)

se.p<-results.p$est-results.p$ci.lb
t<-results.p$est/se.p
coef.p<-toformat(results.p$est, decimals)

p.p<-pt(q=t, df=n,lower.tail=FALSE)
se.p<- toformat(se.p, decimals)
se.p<- paste("(",se.p,")",sep="")
se.p<-p.stars(p.p,se.p)

## AK estimation 

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

coef.ak<-estimation[1]
se.ak<-se[1]

p.ak<-pt(q=coef.ak/se.ak, df=n,lower.tail=FALSE)

se.ak<- toformat(se.ak, decimals)
se.ak<- paste("(",se.ak,")",sep="")
se.ak<-p.stars(p.ak,se.ak)
coef.ak<- toformat(coef.ak, decimals)


### 3 PSM

library(weightr)

results<-weightfunct(final$z_treatment_effect, final$z_se, steps = c(0,0.05, 1), mods = NULL,
                     weights = NULL, fe = FALSE, table = FALSE, pval = NULL)


coef.3psm<-toformat(results$adj_est[2], decimals)
p.3psm<-results$p_adj[2]

se.3psm<-results$adj_se[2]
se.3psm<- toformat(se.3psm,decimals)
se.3psm<- paste("(",se.3psm,")",sep="")
se.3psm<-p.stars(p.3psm,se.3psm)


consolidated.all<- data.frame(
                              "P-Uniform*"=c("",coef.p,se.p,""),
                              "AK"=c("",coef.ak,se.ak,""),
                              "3PSM"=c("",coef.3psm,se.3psm,"")
)


names(consolidated.all)<-c("P-uniform*", "AK", "3PSM")


consolidated.tex <- xtable(consolidated.all,
                           caption="Publication BiasAdjusted Estimates")

setwd(diroverleaf)

print(consolidated.tex, file = "tables/metabias.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')
