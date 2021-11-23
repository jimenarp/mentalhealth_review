
gdp<-read.csv('data/raw/imf_gdppercapita.csv') # LOAD File


########################################################################################################################################
################## CLEAN ###############################################################################################################
########################################################################################################################################


clean<-raw

########### Converting types to numeric ################################################################################################
########################################################################################################################################

numericvars<-c('end_sd_con','bas_sd_con','bas_sd_all','end_sd_treatment','end_sd_all',
               'end_mean_con','bas_mean_con','bas_mean_all', 'end_mean_treatment','end_mean_all',
               'se_treatment_effect','treatment_effect',
               'intervention_value','duration_month', 'treatment_n','control_n','all_n','outcome_dummy')
clean<-clean %>% mutate_at(numericvars, as.character) 

clean$intervention_value <- as.numeric(gsub(",","",clean$intervention_value)) #taking out commas of intervention value

clean<-clean %>% mutate_at(numericvars, as.numeric) %>%
  mutate(all_n=ifelse(is.na(all_n), treatment_n+control_n, all_n)) %>%
  filter(include_yn=='y'|include_yn=='Y') 

#a<-clean %>% filter(authors=='Leventhal & Dupere') %>% select(population, control_n,outcome,outcome_dummy, treatment_effect, se_treatment_effect,end_sd_con,end_mean_con,bas_sd_con,bas_mean_con)



########### Standarize treatment effects ################################################################################################
########################################################################################################################################

#a<-clean %>% filter( standarized!=1& is.na(end_sd_con)&!is.na(control_n)&!is.na(treatment_n)&!is.na(se_treatment_effect)) # samples to double-check
#b<-clean %>% filter( is.na(end_sd_con)&!is.na(all_n)&!is.na(se_treatment_effect)) # samples to double-check


# keep wellbeing

clean<-clean %>% filter(outcome_category=='Anxiety'|outcome_category=='Depression'|outcome_category=='Happiness or Satisfaction'|outcome_category=='Stress') %>%
  mutate(intervention_code=ifelse(is.na(intervention_code),archive_name,intervention_code))


clean <- clean %>% 
  #  Keeping t-stat to estimate standarized SE: 
  mutate(t_effect=treatment_effect/se_treatment_effect) %>% 
  
  # identifying standarized SE if it's not flagged
  mutate(standarized=ifelse((is.na(standarized)|standarized==0)&end_mean_con==0&end_sd_con==1,1,standarized)) %>%  
  
  # filling up NA's in standarized:
  mutate(standarized=ifelse(is.na(standarized), 0, standarized)) %>%  
  
  #  If it's standarized, control SD = 1: 
  mutate(end_sd_con=ifelse(standarized==1, 1, end_sd_con)) %>%  
  
  # if no control SD and is dummy, estimate SD with proportions -endline con: 
  mutate(end_sd_con=ifelse(standarized!=1& is.na(end_sd_con)&outcome_dummy==1&!is.na(control_n),((end_mean_con*(1-end_mean_con)/control_n)^0.5)*(control_n^0.5),end_sd_con)) %>% 
  # if no control SD and is dummy, estimate SD with proportions - baseline con: 
  mutate(bas_sd_con=ifelse(standarized!=1& is.na(bas_sd_con)&outcome_dummy==1&!is.na(control_n),((bas_mean_con*(1-bas_mean_con)/control_n)^0.5)*(control_n^0.5),bas_sd_con)) %>% 
  # if no control SD and is dummy, estimate SD with proportions - all con: 
  mutate(end_sd_all=ifelse(standarized!=1& is.na(end_sd_all)&outcome_dummy==1&!is.na(control_n),((end_mean_all*(1-end_mean_all)/control_n)^0.5)*(control_n^0.5),end_sd_all)) %>% 
 
  #estimating end_sd_con from t ttes
  mutate(end_sd_con=ifelse(standarized!=1&is.na(end_sd_con)&!is.na(control_n)&!is.na(treatment_n)&!is.na(se_treatment_effect)&outcome_dummy!=1,
         ((se_treatment_effect^2)*control_n*treatment_n/(treatment_n+control_n))^0.5, end_sd_con))%>% #assuming same SD for control and treatment
  #transformation bas_sd_con
  # choosing se_input to standarize treatment effect on this rank: end_sd_con, bas_sd_con, end_sd_all, bas_sd_all, en_sd_treatment
  mutate(se_input=ifelse(!is.na(end_sd_con), end_sd_con,  
                                         ifelse(!is.na(bas_sd_con),bas_sd_con,
                                                ifelse(!is.na(end_sd_all),end_sd_all,
                                                       ifelse(!is.na(bas_sd_all),bas_sd_all,
                                                              ifelse( !is.na(end_sd_treatment), end_sd_treatment, NA)))))) %>% 
  
  # choosing mean of sample similar corresponding to the SD chosen to standarize treatment effect 
  mutate(mean_input=ifelse(!is.na(end_mean_con), end_mean_con, 
                           ifelse(!is.na(bas_mean_con),bas_mean_con,
                                  ifelse(!is.na(end_mean_all),end_mean_all,
                                         ifelse(!is.na(bas_mean_all),bas_mean_all,
                                                ifelse(!is.na(end_mean_treatment), end_mean_treatment, NA)))))) %>%
  
  # Standarization: if standarized keep treatment effect
  mutate(z_treatment_effect=ifelse(standarized==1,treatment_effect, NA)) %>% 
  
  # Standarized using se_input
  mutate(z_treatment_effect=ifelse((standarized==0)&!is.na(se_input),treatment_effect/se_input, z_treatment_effect ))%>% 
  
  # Hedges transformation - Adjustment on sample size
  mutate(z_treatment_effect=ifelse(!is.na(treatment_n)&!is.na(control_n),
                                   z_treatment_effect*(1- 3/(4*treatment_n + 4*control_n - 9)),z_treatment_effect)) %>% 
  
  # Standarization: if standarized keep standard error
  mutate(z_se=ifelse(standarized==1,se_treatment_effect, NA)) %>% 
  
  # Standarize error using se_input
  mutate(z_se=ifelse(standarized==0&!is.na(se_input) &t_effect!=0, z_treatment_effect/t_effect, z_se )) %>% 
  
  # if outcome is negative, change sign:
  mutate(z_treatment_effect=ifelse(outcome_negative==1&!is.na(z_treatment_effect), -1*z_treatment_effect,z_treatment_effect))    
#b<-clean %>% filter(authors=='Leventhal & Dupere') %>% select(population, control_n,outcome,outcome_dummy, treatment_effect, se_treatment_effect,end_sd_con,                                                             end_mean_con,bas_sd_con,bas_mean_con, mean_input, se_input, z_treatment_effect, z_se,t_effect)

########### Classifying treatment types ################################################################################################
########################################################################################################################################

clean<-clean %>% 
  mutate(treatment_type=as.character(treatment_type)) %>%
  mutate(treatment_type=ifelse(treatment_type=='Cash Transfer'&transfer_type=='Unconditional','UCT',
                                 ifelse(treatment_type=='Cash Transfer'&transfer_type=='Conditional','CCT',treatment_type))) %>%
  mutate(int=case_when(treatment_type=='UCT' ~ 'UCT', 
                                  treatment_type=='CCT' ~ 'CCT',
                                  treatment_type=='Debt relief' ~ 'Debt',
                                  treatment_type=='Graduation Program' ~ 'Grad', 
                                  treatment_type=='Asset Transfer' ~ 'Asset',
                                  treatment_type=='Lottery'~'Lottery',
                                  treatment_type=='Heathcare'~'Health',
                                  treatment_type=='Savings'~'Savings',
                                  treatment_type=='Neighborhood'~'Neighborhood',
                       treatment_type=='Healthcare'~'Healthcare'))

clean <- fastDummies::dummy_cols(clean, select_columns = "int")


# keep wellbeing

clean<-clean %>% filter(outcome_category=='Anxiety'|outcome_category=='Depression'|outcome_category=='Happiness or Satisfaction'|outcome_category=='Stress')

#a<- clean %>% filter(is.na(z_treatment_effect)) %>% select(title,z_treatment_effect,treatment_effect,se_input, standarized,outcome_dummy, end_sd_treatment,
#                                                           outcome,  end_sd_con,bas_sd_con,bas_sd_all,end_sd_treatment, end_mean_con, control_n) # samples to double-check
#b<- clean  %>% filter(is.na(z_treatment_effect))  %>% select(authors,title,z_treatment_effect,treatment_effect,se_input, standarized,outcome_dummy, end_sd_treatment,
#                                                          outcome,  end_sd_con,bas_sd_con,bas_sd_all,end_sd_treatment, end_mean_con, control_n) # samples to double-check



########### Select variables and filter ################################################################################################
########################################################################################################################################


# Filter obs with standarized treatment effect 

incomplete <- clean %>% filter(is.na(z_treatment_effect))
  
clean<-clean %>% filter(!is.na(z_treatment_effect))


# Select variables

final<-clean %>% 
  filter(include_yn!='N' & !is.na(z_treatment_effect)&index_with_components!=1) %>%
  select(title, year, baseline_year, country, treatment_type, duration_month, 
         z_treatment_effect, intervention_value, all_n,treatment_n, control_n, se_treatment_effect, population, depression_yn, 
         outcome, outcome_negative,treatment_type,transfer_type, outcome_category, archive_name, 
         int_Asset,            
         int_CCT, int_Grad,int_Healthcare, int_Lottery, int_Neighborhood,target,
         int_Savings, int_UCT, treatment=int, age, age_range, female_share, authors, duration, population,z_se, LMIC,  lumpsum, intervention_code) 
  



########### Creating and transforming variables for analysis  ################################################################################################
########################################################################################################################################


final<-final %>% mutate(term=case_when(duration_month<=3 ~ 'less than 3 months',
                                       duration_month>3&duration_month<=12 ~ '3-12 months',
                                       duration_month>3&duration_month>12&duration_month<=24 ~ '12-24 months',
                                       duration_month>3&duration_month>24 ~ '+24 months'))

final<-final %>% mutate(female_share=as.character(female_share)) %>%
  mutate(female_share=str_replace_all(female_share, "%", "")) %>%
  mutate(female_share=as.numeric(female_share))  %>%
  #mutate(female_share = female_share/100) %>%
  mutate(intervention_value_k=intervention_value/1000) %>%
  mutate(intervention_value_10k=intervention_value/10000) %>%
  mutate(intervention_value_100k=intervention_value/100000) %>%
  mutate(duration_years=duration_month/12) %>%
  mutate(outcome_category=as.character(outcome_category)) %>%
  mutate(outcome=as.character(outcome)) %>%
  mutate(archive_name=as.character(archive_name)) %>%
  mutate(id=paste(as.character(authors), as.character(year), sep=' ')) %>%
  mutate(invSE=(1/z_se)) 



########### Final filter  ################################################################################################
########################################################################################################################################
  
    

tmpdata <- final %>%   mutate(id=paste(as.character(authors), as.character(year), sep=' ')) %>%
  filter(id=="Baird et al. 2013")

########### GDP ################################################################################################
########################################################################################################################################

names(gdp)[1]<-'gdp'
gdp<-gdp %>% mutate_all(as.character)
gdp<-melt(id.vars='gdp', gdp)
gdp <- gdp %>% mutate(variable=substr(as.character(variable), 2,5)) %>%
  mutate(gdp=as.character(gdp)) %>% mutate(gdp=ifelse(gdp=="China, People's Republic of","China", gdp)) %>%
  mutate(gdp=ifelse(gdp=="Congo, Dem. Rep. of the", "Congo", gdp)) %>% 
  mutate(gdp=ifelse(gdp=="South Sudan, Republic of", "South Sudan", gdp))

gdpcopy<-gdp
gdp<-gdp %>% mutate(country_year=paste(gdp, variable))
gdp <- gdp %>% select(country_year, value) %>% mutate(value=as.numeric(value))
names(gdp)<-c('country_year','gdppercapita')

final<-final %>% mutate(country=as.character(country)) %>% 
  mutate(country=ifelse(country=="Burkina Fasso", "Burkina Faso", country))  %>%
  mutate(country=ifelse(country=="Democratic Republic of Congo", "Congo", country)) %>%
  mutate(country=ifelse(country=="Phillipines", "Philippines", country )) %>%
  mutate(country=ifelse(country=="UK", "United Kingdom", country )) %>%
  mutate(country=ifelse(country=="Uk", "United Kingdom", country )) %>%
  mutate(country=ifelse(country=="USA", "United States", country )) 

final<-mutate(final, country_year=paste(country, baseline_year))
final<-merge(final, gdp, by='country_year', all.x=TRUE)


gdp1<-gdpcopy %>% mutate(value=as.numeric(value)) %>% 
  filter(variable=='2009') %>% filter(gdp=='Ethiopia'|gdp=='Ghana'|gdp=="Honduras"|gdp=="Pakistan"|gdp=="Peru")
mean(gdp1$value)


gdp1<-gdpcopy %>% mutate(value=as.numeric(value)) %>% 
  filter(variable=='2009') %>% filter(gdp=='Ethiopia'|gdp=='Ghana'|gdp=="India"|gdp=="Peru")
mean(gdp1$value)


gdp1<-gdpcopy %>% mutate(value=as.numeric(value)) %>% 
  filter(variable=='2009') %>% filter(gdp=='Ethiopia'|gdp=='Ghana'|gdp=="India")
mean(gdp1$value)



final <- final %>%
  mutate(gdppercapita = ifelse(country_year=="Ethiopia, Ghana, Honduras, India, Pakistan, Peru 2009",4264.183, gdppercapita)) %>% 
  mutate(gdppercapita = ifelse(country_year=="Ethiopia, Ghana, Honduras, India, Peru 2009",4370.133, gdppercapita)) %>% 
  mutate(gdppercapita = ifelse(country_year=="Ethiopia, Ghana, India, 2009",2897.83, gdppercapita))  %>% 
  mutate(gdppercapita = ifelse(country_year=="Ethiopia, Ghana, India 2009",2897.83, gdppercapita)) 

final<-final %>% select(-country_year)
  
final<-final %>% 
  mutate(title=as.character(title)) %>%
  mutate(population=as.character(population)) %>%
  mutate(depression_yn=as.character(depression_yn)) %>%
  mutate(transfer_type=as.character(transfer_type)) %>%
  mutate(age_range =as.character(age_range)) %>%
  mutate(duration =as.character(duration)) %>%
  mutate(authors =as.character(authors)) %>%
  mutate(ST=ifelse(duration<=12,1,0)) %>% 
  filter(treatment!='Savings')

  
save(final, file= 'data/final/final.Rdata')

save.dta13(final, 'data/final/final.dta')


complete<-raw %>% 
  mutate(treatment_type=as.character(treatment_type)) %>%
  mutate(treatment_type=ifelse(treatment_type=='Cash Transfer'&transfer_type=='Unconditional','UCT',
                               ifelse(treatment_type=='Cash Transfer'&transfer_type=='Conditional','CCT',treatment_type))) %>%
  mutate(int=case_when(treatment_type=='UCT' ~ 'UCT', 
                       treatment_type=='CCT' ~ 'CCT',
                       treatment_type=='Debt relief' ~ 'Debt',
                       treatment_type=='Graduation Program' ~ 'Grad', 
                       treatment_type=='Asset Transfer' ~ 'Asset',
                       treatment_type=='Lottery'~'Lottery',
                       treatment_type=='Heathcare'~'Health',
                       treatment_type=='Savings'~'Savings',
                       treatment_type=='Neighborhood'~'Neighborhood',
                       treatment_type=='Healthcare'~'Healthcare')) %>% filter(outcome_category!='None')%>% filter(outcome_category!='Other')
save(complete, file= 'data/final/complete.Rdata')

