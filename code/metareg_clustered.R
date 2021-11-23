setwd(dir)
load(file='data/final/final.Rdata')

final<-final %>% 
  mutate(int_All=1) %>% 
  mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))

  
final<-final %>% 
  #mutate(intervention_gdppercapita = log(intervention_value/gdppercapita))%>% 
 # mutate(intervention_gdppercapita = intervention_value/(gdppercapita*1000))%>% 
 mutate(intervention_gdppercapita = intervention_value/(gdppercapita))%>% 
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


for(i in c('int_All', 'int_UCT','int_CCT', 'int_Neighborhood', 'int_Grad', 'int_Lottery', 'int_Asset', 'int_Healthcare')) {
    tryCatch({
      temp_data<-final[final[[i]]==1,]
     
      temp_reg<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, data=temp_data, method='SJ',
                    mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years+age_missing+female_missing+intervention_gdppercapita_missing+duration_years_missing )
     
      if (temp_reg$p<length(unique(temp_data$intervention_code))){
        temp_reg<-robust(temp_reg, cluster=temp_data$intervention_code)
      }
      temp<-as.data.frame(temp_reg$beta)
      temp <- cbind(variable = rownames(temp), temp)
      temp<-temp$variable
      for(j in c('intrcpt','age', 'LMIC','lumpsum', 'female_share', 'intervention_gdppercapita', 'duration_years')){
        a<-match(j,temp)
        if(is.na(a)){
          temp_coef='---'
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
      
  consolidated<- data.frame("variable"=c("","Constant","","","Age","","","Low-/Middle-","Income Country","","Female Share","","","Lump Sum","","","Intervention Value","(as % of GDP per capita)","","Delay Intervention-Survey","(Years)","","Observations/Studies","" ),
                                "All"=c("",coef_intrcpt,se_intrcpt,'',coef_age,se_age,"",coef_LMIC, se_LMIC, '', coef_female_share, se_female_share, '',
                                        coef_lumpsum, se_lumpsum, '', coef_intervention_gdppercapita, se_intervention_gdppercapita, '', coef_duration_years, se_duration_years, '',ns,''))
  
  names(consolidated)<-c('variable',i)
                                
  temp_name_cons<-paste(paste('consolidated.',i,sep=''))
  assign(temp_name_cons, consolidated)
  
  print(i)
    }, error=function(e){})
}



consolidated.final<-cbind(consolidated.int_All,consolidated.int_UCT[,2],consolidated.int_CCT[,2],consolidated.int_Neighborhood[,2],consolidated.int_Grad[,2],consolidated.int_Lottery[,2],consolidated.int_Asset[,2],consolidated.int_Healthcare[,2])
names(consolidated.final)<-c('','(1)', '(2)','(3)','(4)','(5)','(6)', '(7)','(8)')

metareg.tex <- xtable(consolidated.final,
                      caption="Determinants of Treatment Effects by Intervention Type",
                      align=c(
                        "l{0.01cm}",
                        "l{0.1cm}",
                        "c{0.5cm}",
                        "c{0.5cm}",
                        "c{0.5cm}",
                        "c{0.5cm}",
                        "c{0.5cm}",
                        "c{0.5cm}","c{0.5cm}","c{0.5cm}"))

setwd(diroverleaf)


addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0
addtorow$command <- c(" &  \\shortstack{All \\\\ Interventions} & \\shortstack{Unconditional \\\\ Cash \\\\ Transfer} & \\shortstack{Conditional \\\\ Cash \\\\ Transfer}  & \\shortstack{Housing \\\\ Voucher} & \\shortstack{Poverty \\\\ Graduation\\\\ Program} &  \\shortstack{Lottery\\\\ Win}  & \\shortstack{Asset\\\\ Transfer}  & \\shortstack{Health\\\\ Insurance \\\\ Provision}   \\\\  ")

print(metareg.tex, file = "tables/metareg_clustered.tex", compress = FALSE, 
      include.rownames=FALSE,
      add.to.row = addtorow,
      
      size = "small",
      caption.placement =  "top" , justify ='centre', floating=FALSE)



