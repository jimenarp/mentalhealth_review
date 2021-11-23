setwd(dir)

load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')

decimals<-3
library(weightr)

final<-final %>% filter(!is.na(z_se))%>% filter(!is.na(z_treatment_effect))
final<- final %>% mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) 

n<-nrow(final)
n1<-nrow(final_one)
for(i in c('out_All', 'out_Depression','out_Stress_Anx', 'out_Happy_Sat')) {
  temp_data<-final[final[[i]]==1,]
  print(i)
  
  # naive estimate
  naive<-rma(yi=z_treatment_effect, sei=z_se, weighted=TRUE, data=temp_data, method='SJ',slab=archive_name)
  naive_coef<-naive$beta[1,1]
  naive_coef<- toformat(naive_coef, decimals)
  naive<-robust(naive, cluster=temp_data$id)
  naive_se<-naive$se
  naive_pval<-naive$pval
  naive_se<- toformat(naive_se, decimals)
  naive_se<- paste("(",naive_se,")",sep="")
  naive_se<-p.stars(naive_pval, naive_se) 
  temp_name_naive_coef<-paste('naive_coef_',i,sep='')
  temp_name_naive_se<-paste('naive_se_',i,sep='')
  assign(temp_name_naive_coef, naive_coef)
  assign(temp_name_naive_se, naive_se)
  
  
  # puniform estimate
  puni<-puni_star(yi=temp_data$z_treatment_effect, vi=temp_data$z_se , method ='ML' , side= "right", alpha = 0.05)#effect sizes are in the right tail (positive)
  puni_se<-puni$est-puni$ci.lb
  t<-puni$est/puni_se
  puni_coef<-toformat(puni$est, decimals)
  puni_pval<-pt(q=t, df=n,lower.tail=FALSE)
  puni_se<- toformat(puni_se, decimals)
  puni_se<- paste("(",puni_se,")",sep="")
  puni_se<-p.stars(puni_pval,puni_se)
  temp_name_puni_coef<-paste('puni_coef_',i,sep='')
  temp_name_puni_se<-paste('puni_se_',i,sep='')
  assign(temp_name_puni_coef, puni_coef)
  assign(temp_name_puni_se, puni_se)
  
  #AK estimate
  lower.b = c(-Inf,0,0)
  upper.b= c(Inf,Inf,Inf)
  theta0 <- c(0.11, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig). Review if these are reasonable values 
  stepsize <- 10^(-6)
  cutoffs = c(1.96)
  symmetric = 1
  data_temp_ak<-temp_data %>% select(z_treatment_effect, z_se) %>% filter(!is.na(z_se))%>% filter(!is.na(z_treatment_effect))
  
  LLH_only <-function (Psi) {
    A<-VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1,2)],  1),cutoffs,symmetric, data_temp_ak$z_treatment_effect,  data_temp_ak$z_se , normalizedsign = 0 );
    return (A$LLH) 
  } 
  findmin <- nlminb(objective=LLH_only, start=theta0,lower=lower.b,upper=upper.b) #takes only the Log Likelihood but not other parts of the object
  ak<-findmin$par # the three parameters that maximize the log likelihood function. using this LL and this values to find the most likely
  se <- robust_se(findmin$par, stepsize)
  ak_coef<-ak[1]
  ak_se<-se[1]
  ak_pval<-pt(q=ak_coef/ak_se, df=n,lower.tail=FALSE)
  ak_se<- toformat(ak_se, decimals)
  ak_se<- paste("(",ak_se,")",sep="")
  ak_se<-p.stars(ak_pval,ak_se)
  ak_coef<- toformat(ak_coef, decimals)
  temp_name_ak_coef<-paste('ak_coef_',i,sep='')
  temp_name_ak_se<-paste('ak_se_',i,sep='')
  assign(temp_name_ak_coef, ak_coef)
  assign(temp_name_ak_se, ak_se)
  
  #3psm
  
  tpsm<-weightfunct(temp_data$z_treatment_effect, temp_data$z_se, steps = c(0,0.05, 1), mods = NULL,
                    weights = NULL, fe = FALSE, table = FALSE, pval = NULL)
  tpsm_coef<-toformat(tpsm$adj_est[2], decimals)
  tpsm_pval<-tpsm$p_adj[2]
  tpsm_se<-tpsm$adj_se[2]
  temp_name_tpsm_coef<-paste('tpsm_coef_',i,sep='')
  temp_name_tpsm_se<-paste('tpsm_se_',i,sep='')
  if (tpsm_se !='NaN'){
    tpsm_se<- toformat(tpsm_se,decimals)
    tpsm_se<- paste("(",tpsm_se,")",sep="")
    tpsm_se<-p.stars(tpsm_pval,tpsm_se)
  } else {
    tpsm_se='---'
  }
  assign(temp_name_tpsm_coef, tpsm_coef)
  assign(temp_name_tpsm_se, tpsm_se)
  
}


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "Reg 1"=c("",naive_coef_out_All,naive_se_out_All,""),
                              "Reg 2"=c("",puni_coef_out_All,puni_se_out_All,""),
                              "Reg 3"=c("",ak_coef_out_All,ak_se_out_All,""),
                              "Reg 4"=c("",tpsm_coef_out_All,tpsm_se_out_All,"")
                              
)


consolidated.Depression<- data.frame("group"=c("","Depression","","" ),
                              "Reg 1"=c("",naive_coef_out_Depression,naive_se_out_Depression,""),
                              "Reg 2"=c("",puni_coef_out_Depression,puni_se_out_Depression,""),
                              "Reg 3"=c("",ak_coef_out_Depression,ak_se_out_Depression,""),
                              "Reg 4"=c("",tpsm_coef_out_Depression,tpsm_se_out_Depression,"")
                              
)


consolidated.Stress<- data.frame("group"=c("","Stress or Anxiety","","" ),
                              "Reg 1"=c("",naive_coef_out_Stress_Anx,naive_se_out_Stress_Anx,""),
                              "Reg 2"=c("",puni_coef_out_Stress_Anx,puni_se_out_Stress_Anx,""),
                              "Reg 3"=c("",ak_coef_out_Stress_Anx,ak_se_out_Stress_Anx,""),
                              "Reg 4"=c("",tpsm_coef_out_Stress_Anx,tpsm_se_out_Stress_Anx,"")
                              
)

consolidated.Happiness<- data.frame("group"=c("","Happiness","","" ),
                                  "Reg 1"=c("",naive_coef_out_Happy_Sat,naive_se_out_Happy_Sat,""),
                                  "Reg 2"=c("",puni_coef_out_Happy_Sat,puni_se_out_Happy_Sat,""),
                                  "Reg 3"=c("",ak_coef_out_Happy_Sat,ak_se_out_Happy_Sat,""),
                                  "Reg 4"=c("",tpsm_coef_out_Happy_Sat,tpsm_se_out_Happy_Sat,"")
                                  
)



consolidated_final<-rbind(consolidated.all, consolidated.Depression ,consolidated.Stress,consolidated.Happiness )

names(consolidated_final)<-c("","(1)","(2)","(3)","(4)")

pooled.tex <- xtable(consolidated_final,
                     caption="Publication Bias Adjustment by Outcome Variable",
                     align=c(
                       "l{0.01cm}",
                       "l{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}"),format.args = list(digits = 3))


addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0



addtorow$command <- c(" &  \\shortstack{Naïve\\\\ Estimation} & P-uniform^* & Andrews \\& Kasy & Vevea \\& Hedges  \\\\  ")

setwd(diroverleaf)


print(pooled.tex, file = "tables/bias_out.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
      # sanitize.colnames.function=bold, 
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" , justify ='centre', floating=FALSE)
# 
