setwd(dir)

load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')

decimals<-3
library(weightr)

final<-final %>% filter(!is.na(z_se))%>% filter(!is.na(z_treatment_effect))

n<-nrow(final)
n1<-nrow(final_one)
final<- final %>% mutate(int_All=1)
for(i in c('int_All', 'int_UCT','int_CCT', 'int_Neighborhood', 'int_Grad', 'int_Lottery', 'int_Asset', 'int_Healthcare')) {
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
                              "Reg 1"=c("",naive_coef_int_All,naive_se_int_All,""),
                              "Reg 2"=c("",puni_coef_int_All,puni_se_int_All,""),
                              "Reg 3"=c("",ak_coef_int_All,ak_se_int_All,""),
                              "Reg 4"=c("",tpsm_coef_int_All,tpsm_se_int_All,"")
                              
)




consolidated.UCT<- data.frame("group"=c("","Unconditional Cash Transfer","","" ),
                              "Reg 1"=c("",naive_coef_int_UCT,naive_se_int_UCT,""),
                              "Reg 2"=c("",puni_coef_int_UCT,puni_se_int_UCT,""),
                              "Reg 3"=c("",ak_coef_int_UCT,ak_se_int_UCT,""),
                              "Reg 4"=c("",tpsm_coef_int_UCT,tpsm_se_int_UCT,"")
                              
)


consolidated.CCT<- data.frame("group"=c("","Conditional Cash Transfer","","" ),
                              "Reg 1"=c("",naive_coef_int_CCT,naive_se_int_CCT,""),
                              "Reg 2"=c("",puni_coef_int_CCT,puni_se_int_CCT,""),
                              "Reg 3"=c("",ak_coef_int_CCT,ak_se_int_CCT,""),
                              "Reg 4"=c("",tpsm_coef_int_CCT,tpsm_se_int_CCT,"")
                              
)

consolidated.Lottery<- data.frame("group"=c("","Lottery Win","","" ),
                                       "Reg 1"=c("",naive_coef_int_Lottery,naive_se_int_Lottery,""),
                                       "Reg 2"=c("",puni_coef_int_Lottery,puni_se_int_Lottery,""),
                                       "Reg 3"=c("",ak_coef_int_Lottery,ak_se_int_Lottery,""),
                                       "Reg 4"=c("",tpsm_coef_int_Lottery,tpsm_se_int_Lottery,"")
                                       
)

consolidated.Neighborhood<- data.frame("group"=c("","Housing Voucher","","" ),
                              "Reg 1"=c("",naive_coef_int_Neighborhood,naive_se_int_Neighborhood,""),
                              "Reg 2"=c("",puni_coef_int_Neighborhood,puni_se_int_Neighborhood,""),
                              "Reg 3"=c("",ak_coef_int_Neighborhood,ak_se_int_Neighborhood,""),
                              "Reg 4"=c("",tpsm_coef_int_Neighborhood,tpsm_se_int_Neighborhood,"")
                              
)


consolidated.Grad<- data.frame("group"=c("","Poverty Graduation Program","","" ),
                                       "Reg 1"=c("",naive_coef_int_Grad,naive_se_int_Grad,""),
                                       "Reg 2"=c("",puni_coef_int_Grad,puni_se_int_Grad,""),
                                       "Reg 3"=c("",ak_coef_int_Grad,ak_se_int_Grad,""),
                                       "Reg 4"=c("",tpsm_coef_int_Grad,tpsm_se_int_Grad,"")
                                       
)


consolidated.Asset<- data.frame("group"=c("","Asset Transfer","","" ),
                               "Reg 1"=c("",naive_coef_int_Asset,naive_se_int_Asset,""),
                               "Reg 2"=c("",puni_coef_int_Asset,puni_se_int_Asset,""),
                               "Reg 3"=c("",ak_coef_int_Asset,ak_se_int_Asset,""),
                               "Reg 4"=c("",tpsm_coef_int_Asset,tpsm_se_int_Asset,"")
                               
)


consolidated.Healthcare<- data.frame("group"=c("","Health Insurance Provision","","" ),
                                "Reg 1"=c("",naive_coef_int_Healthcare,naive_se_int_Healthcare,""),
                                "Reg 2"=c("",puni_coef_int_Healthcare,puni_se_int_Healthcare,""),
                                "Reg 3"=c("",ak_coef_int_Healthcare,ak_se_int_Healthcare,""),
                                "Reg 4"=c("",tpsm_coef_int_Healthcare,tpsm_se_int_Healthcare,"")
                                
)



consolidated_final<-rbind(consolidated.all, consolidated.UCT ,consolidated.CCT,consolidated.Neighborhood,  consolidated.Grad, consolidated.Lottery,consolidated.Asset,consolidated.Healthcare )

names(consolidated_final)<-c("","(1)","(2)","(3)","(4)")

pooled.tex <- xtable(consolidated_final,
                     caption="Publication Bias Adjustment by Internvention Type",
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



addtorow$command <- c(" &  \\shortstack{Naïve\\\\ Estimation} & P-uniform^* & Andrews \\& Kasy & Veva \\& Hedges  \\\\  ")

setwd(diroverleaf)


print(pooled.tex, file = "tables/bias_int.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
      # sanitize.colnames.function=bold, 
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" , justify ='centre', floating=FALSE)
# 

