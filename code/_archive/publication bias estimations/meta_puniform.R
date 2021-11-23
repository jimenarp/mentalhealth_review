setwd(dir)

load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')

### PUNIFORM #####


### many outcomes per study ####


final<-final %>% filter(!is.na(z_se))

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


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
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
      caption.placement =  "top" , justify ='centre')

rm(coefP,seP,coefLNP,seLNP,coefLN1MINP,seLN1MINP,coefKS)

puni_star(yi=final$z_treatment_effect, vi=final$z_se , method = 'P' , side= "right", alpha = 0.05)

for (i in c('P','LNP','ML')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , ML
  results.temp<-puni_star(yi=final$z_treatment_effect, vi=final$z_se , method = i , side= "right", alpha = 0.05)#effect sizes are in the right tail (positive)
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  se.name<-paste('se',i, sep='')
  se.temp<-results.temp$est-results.temp$ci.lb
  se.temp<- toformat(se.temp, decimals)
  se.temp<- paste("(",se.temp,")",sep="")
  #p.name<-paste('p',i, sep='')
  #p.temp<-results.temp$pval.0
  #assign(p.name,p.temp)
  #se.temp<-p.stars(p.temp, se.temp)
  assign(se.name,se.temp)
}


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "ML"=c("",coefML,seML,"")
)



setwd(diroverleaf)


punistar_all.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform*) Adjusted Estimates")


print(punistar_all.tex, file = "tables/punistar_all.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')


final<-final %>% filter(!is.na(z_se))
temp<-metagen(z_treatment_effect,
              z_se,
              data = final,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")



eggers.test(x = temp)



setwd(diroverleaf)
pdf(file = 'figures/pcurve_all.pdf') 
test<-pcurve(temp)

dev.off() 

#### one outcome per study ####
res <- rma(yi=one_outcome, sei=one_outcome_sd, slab=archive_name, weighted=TRUE, data=final_one, method='SJ')


for (i in c('P','LNP','LN1MINP','KS')){
  results.temp<-puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = i , side= "right")
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




consolidated.one<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "LN1MINP"=c("",coefLN1MINP,seLN1MINP,""),
                              "KS"=c("",coefKS,"-","")
)


setwd(diroverleaf)


puniform_one.tex <- xtable(consolidated.one,
                           caption="Publication Bias (P-Uniform) Adjusted Estimates")


print(puniform_one.tex, file = "tables/puniform_one.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')

rm(coefP,seP,coefLNP,seLNP,coefLN1MINP,seLN1MINP,coefKS)


for (i in c('P','LNP','ML')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , Kolmogorov-Smirnov 
  results.temp<-puni_star(yi=final_one$one_outcome, vi=final_one$one_outcome_sd  , method = i , side= "right", alpha=0.05) #effect sizes are in the right tail (positive)
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  se.name<-paste('se',i, sep='')
  se.temp<-results.temp$est-results.temp$ci.lb
  se.temp<- toformat(se.temp, decimals)
  se.temp<- paste("(",se.temp,")",sep="")
  #p.name<-paste('p',i, sep='')
  #p.temp<-results.temp$pval.0
  #assign(p.name,p.temp)
  #se.temp<-p.stars(p.temp, se.temp)
  assign(se.name,se.temp)
}


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "ML"=c("",coefML,seML,"")
)



setwd(diroverleaf)


punistar_one.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform)* Adjusted Estimates")


print(punistar_one.tex, file = "tables/punistar_one.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')




temp<-metagen(one_outcome,
              one_outcome_sd,
              data = final_one,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")


eggers.test(x = temp)



setwd(diroverleaf)
pdf(file = 'figures/pcurve_one.pdf') 
test<-pcurve(temp)

dev.off() 