setwd(dir)
source("code/aux/McShane2016_supplement/pcurve.meta.function.R") # Simonsohn (2015) code
source("code/aux/McShane2016_supplement/selection.meta.functions.R") # McShane (2016) code

load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')


library(weightr)

results<-weightfunct(final$z_treatment_effect, final$z_se, steps = c(0,0.05, 1), mods = NULL,
                     weights = NULL, fe = FALSE, table = FALSE, pval = NULL)

names<-rbind('Tau^2 (estimated amount of total heterogeneity)','Adjusted intercept', '0 < p < 0.05','0.05 < p < 1')

coefs<-toformat(results$adj_est, decimals)
ps<-results$p_adj
ses<-results$adj_se
ses<- toformat(ses,decimals)
ps<- toformat(ps,decimals)

consolidated<-cbind(names,coefs,ses,ps)


consolidated<-as.data.frame(consolidated[-1,])
names(consolidated)<-c('estimator','est','se','pval')
consolidated.tex <- xtable(consolidated,
                           caption="Publication Bias (Vevea and Hedges Weight Function) Adjusted Estimates")

setwd(diroverleaf)

print(consolidated.tex, file = "tables/vevea.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')

temp_se<-p.stars(temp_p, temp_se)



#remotes::install_github("JConigrave/msemtools")
library(msemtools)
theta0 <- c(0.11, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig)

copy<-final %>% mutate(tstat=z_treatment_effect/z_se) %>% filter(!is.na(tstat))
select<-dplyr::select
copy<-copy %>% select(z_treatment_effect, z_se, all_n, tstat)
results.3PSM <- estimate.onestep.selection.heterogeneous(copy$tstat,copy$all_n, copy$all_n, 0.05, theta0)

