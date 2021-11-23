setwd(dir)
load(file='data/final/final.Rdata')

final<-final %>% mutate(int_All=1) %>% 
  mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))

  
final<-final %>%
  mutate(intervention_gdppercapita = log(intervention_value/gdppercapita))%>% 
  #mutate(intervention_gdppercapita = intervention_value/(gdppercapita*1000))%>% 
  mutate(age_missing=ifelse(is.na(age),1,0)) %>% 
  mutate(age=ifelse(is.na(age),0,age)) %>%
  mutate(female_missing=ifelse(is.na(female_share),1,0)) %>% 
  mutate(female_share=ifelse(is.na(female_share),0,female_share)) %>%
  mutate(intervention_gdppercapita_missing=ifelse(is.na(intervention_gdppercapita),1,0)) %>% 
  mutate(intervention_gdppercapita=ifelse(is.na(intervention_gdppercapita),0,intervention_gdppercapita)) %>%
  mutate(duration_years_missing=ifelse(is.na(duration_years),1,0)) %>% 
  mutate(duration_years=ifelse(is.na(duration_years),0,duration_years)) %>%
  mutate(intervention_peryear_gddpercapita=intervention_gdppercapita/duration_years) %>% 
  mutate(intervention_peryear_gddpercapita=ifelse(intervention_peryear_gddpercapita=='Inf',NA,intervention_peryear_gddpercapita)) %>% 
  mutate(intervention_peryear_gddpercapita_missing=ifelse(is.na(intervention_peryear_gddpercapita),1,0)) %>%
  mutate(intervention_peryear_gddpercapita=ifelse(is.na(intervention_peryear_gddpercapita),0,intervention_peryear_gddpercapita)) 


for(i in c('out_All','out_Depression', 'out_Stress_Anx','out_Happy_Sat')) {
    tryCatch({
      temp_data<-final[final[[i]]==1,]
      temp_reg<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, data=temp_data, method='SJ',
                    mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years+age_missing+female_missing+intervention_gdppercapita_missing+duration_years_missing )
      if (temp_reg$p<length(unique(temp_data$id))){
        temp_reg<-robust(temp_reg, cluster=temp_data$id)
      }
      temp<-as.data.frame(temp_reg$beta)
      temp <- cbind(variable = rownames(temp), temp)
      temp<-temp$variable
      for(j in c('intrcpt','age', 'LMIC','lumpsum', 'female_share', 'intervention_gdppercapita', 'duration_years')){
        a<-match(j,temp)
        if(is.na(a)){
          temp_coef='-'
          temp_se=''
        }else{
          position<-match(j,temp)
          temp_coef<-toformat(temp_reg$beta[position,1], decimals)
         
          temp_se<-toformat(temp_reg$se[position], decimals)
          temp_p<-temp_reg$pval[position]
          temp_se<- paste("(",temp_se,")",sep="")
          temp_se<-p.stars(temp_p, temp_se)
        }
        temp_name_coef<-paste(paste('coef_',j,sep=''))
        temp_name_se<-paste(paste('se_',j,sep=''))
        assign(temp_name_coef, temp_coef)
        assign(temp_name_se, temp_se)
      } 
      temp_n<-temp_reg$k
      temp_s<-length(unique(temp_data$id))
      ns<-paste('[',temp_n,'/',temp_s,']')
      
  consolidated<- data.frame("variable"=c("","Constant","","","Age","","","LMIC","","","Female Share","","","Lumpsum","","","Intervention Value","(as % of GDP per capita in logs)","","Delay Intervention-Survey","(Years)","","Obs/Studies","" ),
                                "All"=c("",coef_intrcpt,se_intrcpt,'',coef_age,se_age,"",coef_LMIC, se_LMIC, '', coef_female_share, se_female_share, '',
                                        coef_lumpsum, se_lumpsum, '', coef_intervention_gdppercapita, se_intervention_gdppercapita, '', coef_duration_years, se_duration_years, '',ns,''))
  
  names(consolidated)<-c('variable',i)
                                
  temp_name_cons<-paste(paste('consolidated.',i,sep=''))
  assign(temp_name_cons, consolidated)
  
  print(i)
    }, error=function(e){})
}



consolidated.final<-cbind(consolidated.out_All,consolidated.out_Depression[,2],consolidated.out_Stress_Anx[,2],consolidated.out_Happy_Sat[,2])
names(consolidated.final)<-c('','(1)', '(2)','(3)','(4)')

metareg.tex <- xtable(consolidated.final,
                      caption="Determinants of Treatment Effects by Outcome Variable",
                      align=c(
                        "l{0.01cm}",
                        "l{0.1cm}",
                        "c{0.5cm}",
                        "c{0.5cm}",
                        "c{0.5cm}",
                        "c{0.5cm}"))

setwd(diroverleaf)

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0
addtorow$command <- c(" &  \\shortstack{All Mental Health \\\\ Outcomes} & Depression & Stress or Anxiety & Happiness    \\\\  ")


print(metareg.tex, file = "tables/metareg_clustered_outcomes_log.tex", compress = FALSE, 
      include.rownames=FALSE,
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" , justify ='centre', floating=FALSE)



