############## Packages
#Function to check if package exists, load if not
rm(list=ls())
ipak <- function(pkg){
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg)) 
    install.packages(new_pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packs = c("tidyverse","pwr","metafor","devtools","xlsx", "puniform","metafore")

ipak(packs)
devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(meta)
library(metafor)


decimals<-3

p.stars <-function(pval, se) {
  if (pval<=0.001) {
    se<-paste(se, '***',sep='' )
  } else if  (pval>0.001 & pval<=0.05) {
    se<-paste(se, '**',sep='' )
  } else if  (pval>0.05 & pval<=0.1) {
    se<-paste(se, '*',sep='' )
  } else {
    se<-se
  }
}



toformat <-function(number, n) {
  number=as.character(prettyNum(round(number,n),nsmall=n, big.mark=',', format=f))
}

dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data/final"
setwd(dir)
outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/tables"

load(file='final.Rdata')



final<-final %>% filter(age<=24)
final<- final %>% mutate(int_All=1)
final<- final %>% mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))
           

# Random Effects Using Sidik-Jonkman estimator ("SJ")

all_all<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='SJ')
all_UCT<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final %>% filter(int_UCT==1), method='SJ')

#for(i in c('int_All', 'int_CCT', 'int_Neighborhood', 'int_Grad', 'int_Lottery', 'int_Asset', 'int_Healthcare')) {
#  temp_data<-final[final[[i]]==1,]
#  temp_reg<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=temp_data, method='SJ')
#  temp_coef<-temp_reg$beta[1,1]
#  temp_se<-temp_reg$se
#  temp_I2<-temp_reg$I2
#  temp_name_coef<-paste('coef',substring(i, 5 ),sep='_')
#  temp_name_se<-paste('se',substring(i, 5 ),sep='_')
#  temp_name_I2<-paste('I2',substring(i, 5 ),sep='_')
#  assign(temp_name_coef, temp_coef)
#  assign(temp_name_se, temp_se)
#  assign(temp_name_I2, temp_I2)
#}


for(i in c('int_All', 'int_UCT','int_CCT', 'int_Neighborhood', 'int_Grad', 'int_Lottery', 'int_Asset', 'int_Healthcare')) {
  for( j in c('out_All','out_Depression', 'out_Stress_Anx','out_Happy_Sat','out_Other')){
    tryCatch({
      temp_data<-final[final[[i]]==1,]
      temp_data<-temp_data[temp_data[[j]]==1,]
      temp_reg<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=temp_data, method='SJ')
      temp_coef<-temp_reg$beta[1,1]
      temp_coef<- toformat(temp_coef, decimals)
      temp_se<-temp_reg$se
      temp_pval<-temp_reg$pval
      temp_se<- toformat(temp_se, decimals)
      temp_se<- paste("(",temp_se,")",sep="")
      temp_se<-p.stars(temp_pval, temp_se)
      temp_I2<-temp_reg$I2
      temp_I2<- toformat(temp_I2, decimals)
      temp_name_coef<-paste('coef',substring(i, 5 ),substring(j, 5 ),sep='_')
      temp_name_se<-paste('se',substring(i, 5 ),substring(j, 5 ),sep='_')
      temp_name_I2<-paste('I2',substring(i, 5 ),substring(j, 5 ),sep='_')
      temp_n<-temp_reg$k
      temp_s<-length(unique(temp_data$archive_name))
      temp_name_ns<-paste('ns',substring(i, 5 ),substring(j, 5 ),sep='_')
      temp_ns<-paste('[',temp_n,'/',temp_s,']')
      assign(temp_name_coef, temp_coef)
      assign(temp_name_se, temp_se)
      assign(temp_name_I2, temp_I2)
      assign(temp_name_ns, temp_ns)
    }, error=function(e){})
  }
}



consolidated.all<- data.frame("group"=c("","All Interventions","","","" ),
                              "Reg 1"=c("",coef_All_All,se_All_All,ns_All_All,""),
                              "Reg 2"=c("",coef_All_Depression,se_All_Depression,ns_All_Depression,""),
                              "Reg 3"=c("",coef_All_Stress_Anx,se_All_Stress_Anx,ns_All_Stress_Anx,""),
                              "Reg 4"=c("",coef_All_Happy_Sat,se_All_Happy_Sat,ns_All_Happy_Sat,""),
                              "Reg 5"=c("",coef_All_Other,se_All_Other,ns_All_Other,"")
)



consolidated.UCT<- data.frame("group"=c("UCT","","","" ),
                              "Reg 1"=c(coef_UCT_All,se_UCT_All,ns_UCT_All,""),
                              "Reg 2"=c(coef_UCT_Depression,se_UCT_Depression,ns_UCT_Depression,""),
                              "Reg 3"=c('-','-','-',""),
                              "Reg 4"=c('-','-','-',""),
                              "Reg 5"=c('-','-','-',"")
)



consolidated.CCT<- data.frame("group"=c("CCT","","","" ),
                              "Reg 1"=c(coef_CCT_All,se_CCT_All,ns_CCT_All,""),
                              "Reg 2"=c(coef_CCT_Depression,se_CCT_Depression,ns_CCT_Depression,""),
                              "Reg 3"=c(coef_CCT_Stress_Anx,se_CCT_Stress_Anx,ns_CCT_Stress_Anx,""),
                              "Reg 4"=c('-','-','-',""),
                              "Reg 5"=c(coef_CCT_Other,se_CCT_Other,ns_CCT_Other,"")
)


consolidated.Neighborhood<- data.frame("group"=c("Neighborhood","","","" ),
                                       "Reg 1"=c(coef_Neighborhood_All,se_Neighborhood_All,ns_Neighborhood_All,""),
                                       "Reg 2"=c(coef_Neighborhood_Depression,se_Neighborhood_Depression,ns_Neighborhood_Depression,""),
                                       "Reg 3"=c(coef_Neighborhood_Stress_Anx,se_Neighborhood_Stress_Anx,ns_Neighborhood_Stress_Anx,""),
                                       "Reg 4"=c('-','-','-',""),
                                       "Reg 5"=c(coef_Neighborhood_Other,se_Neighborhood_Other,ns_Neighborhood_Other,"")
)


consolidated.Grad<- data.frame("group"=c("Graduation","","","" ),
                               "Reg 1"=c(coef_Grad_All,se_Grad_All,ns_Grad_All,""),
                               "Reg 2"=c(coef_Grad_Depression,se_Grad_Depression,ns_Grad_Depression,""),
                               "Reg 3"=c('-','-','-',""),
                               "Reg 4"=c('-','-','-',""),
                               "Reg 5"=c(coef_Grad_Other,se_Grad_Other,ns_Grad_Other,"")
)


consolidated.Lottery<- data.frame("group"=c("Lottery","","","" ),
                                  "Reg 1"=c('-','-',"-",""),
                                  "Reg 2"=c('-','-',"-",""),
                                  "Reg 3"=c('-','-',"-",""),
                                  "Reg 4"=c('-','-',"-",""),
                                  "Reg 5"=c("-","-","-","")
)


consolidated.Asset<- data.frame("group"=c("Asset","","","" ),
                                "Reg 1"=c(coef_Asset_All,se_Asset_All,ns_Asset_All,""),
                                "Reg 2"=c(coef_Asset_Depression,se_Asset_Depression,ns_Asset_Depression,""),
                                "Reg 3"=c('-','-','-',""),
                                "Reg 4"=c(coef_Asset_Happy_Sat,se_Asset_Happy_Sat,ns_Asset_Happy_Sat,""),
                                "Reg 5"=c("-","-","-","")
)


consolidated.Healthcare<- data.frame("group"=c("Insurance","","","" ),
                                     "Reg 1"=c('-','-',"-",""),
                                     "Reg 2"=c('-','-',"-",""),
                                     "Reg 3"=c('-','-',"-",""),
                                     "Reg 4"=c('-','-',"-",""),
                                     "Reg 5"=c("-","-","-","")
)


consolidated_final<-rbind(consolidated.all, consolidated.UCT ,consolidated.CCT,consolidated.Neighborhood,  consolidated.Grad, consolidated.Lottery,consolidated.Asset,consolidated.Healthcare )

names(consolidated_final)<-c("","(1)","(2)","(3)","(4)","(5)")
#colnames(consolidated_final) <- paste("\\multicolumn{1}{c|}{", colnames(consolidated_final), "}")

pooled.tex <- xtable(consolidated_final,
                             caption="Pooled  Effect (Random Effects):  Under 24 Years Old",
                     align=c(
                       "c{0.01cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}"))


addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0



addtorow$command <- c(" & Overall Mental Health & Depression & Stress Or Anxiety & Happiness   & Other    \\\\  ")

setwd(outdir_overleaf)


print(pooled.tex, file = "pooled_u24.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
     # sanitize.colnames.function=bold, 
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" )
