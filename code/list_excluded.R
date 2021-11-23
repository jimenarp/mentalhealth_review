

#dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01"
#outdir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data/final"
#dir<-"/Users/haushofer/Dropbox/wellcome_review_2020/analysis"
#outdir<-"/Users/haushofer/Dropbox/wellcome_review_2020/analysis/data/processed"
setwd(dir)
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
  mutate(all_n=ifelse(is.na(all_n), treatment_n+control_n, all_n)) 

# excluded 1 

excluded_1 <- clean %>%
  filter(include_yn!='y'&include_yn!='Y') %>% select (authors, year, title, include_yn, include_why) 



# transforming variables

clean <- clean %>% 
  #  Keeping t-stat to estimate standarized SE: 
  mutate(t_effect=treatment_effect/se_treatment_effect) %>% 
  
  # identifying standarized SE if it's not flagged
  mutate(standarized=ifelse((is.na(standarized)|standarized==0)&end_mean_con==0&end_sd_con==1,1,standarized)) %>%  
  
  # filling up NA's in standarized:
  mutate(standarized=ifelse(is.na(standarized), 0, standarized)) %>%  
  
  #  If it's standarized, control SD = 1: 
  mutate(end_sd_con=ifelse(standarized==1, 1, end_sd_con)) %>%  
  
  # if no control SD and is dummy, estimate SD with proportions: 
  mutate(end_sd_con=ifelse(standarized!=1& is.na(end_sd_con)&outcome_dummy==1&!is.na(control_n),(end_mean_con*(1-end_mean_con)/control_n)^0.5,end_sd_con)) %>% 
  
  #transformation
  mutate(end_sd_con=ifelse(standarized!=1&is.na(end_sd_con)&!is.na(control_n)&!is.na(treatment_n)&!is.na(se_treatment_effect)&outcome_dummy!=1,
                           (se_treatment_effect^2)*control_n*treatment_n/(treatment_n+control_n), end_sd_con))%>% #assuming same SD for control and treatment
  
  # choosing se_input to standarize treatment effect on this rank: end_sd_con, bas_sd_con, end_sd_all, bas_sd_all, en_sd_treatment
  mutate(sd_input=ifelse(!is.na(end_sd_con), end_sd_con,  
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
  mutate(z_treatment_effect=ifelse((standarized==0)&!is.na(sd_input),treatment_effect/sd_input, z_treatment_effect ))%>% 
  
  # Hedges transformation - Adjustment on sample size
  mutate(z_treatment_effect=ifelse(!is.na(treatment_n)&!is.na(control_n),
                                   z_treatment_effect*(1- 3/(4*treatment_n + 4*control_n - 9)),z_treatment_effect)) %>% 
  
  # Standarization: if standarized keep standard error
  mutate(z_se=ifelse(standarized==1,se_treatment_effect, NA)) %>% 
  
  # Standarize error using se_input
  mutate(z_se=ifelse(standarized==0&!is.na(sd_input) &t_effect!=0, z_treatment_effect/t_effect, z_se )) %>% 
  
  # if outcome is negative, change sign:
  mutate(z_treatment_effect=ifelse(outcome_negative==1&!is.na(z_treatment_effect), -1*z_treatment_effect,z_treatment_effect))    

# excluded 2-4 

excluded_2<-clean %>% filter(is.na(z_treatment_effect)) %>% select (authors, year, title, include_yn, include_why) %>% mutate(include_yn='N') %>% mutate(include_why='Not treatment effect (Z)') 


excluded_all_studies<-clean %>% filter(is.na(z_treatment_effect)|is.na(z_se)) %>% select(authors, year, title ) %>% unique()

excluded_all_obs<-clean %>% filter(is.na(z_treatment_effect)|is.na(z_se)) %>% filter(!is.na(se_treatment_effect)|!is.na(se_treatment_effect)) %>% select(authors, year, title, outcome_dummy, treatment_effect, se_treatment_effect, end_sd_con,bas_sd_con,  end_sd_all, bas_sd_all, end_sd_treatment,z_se) %>% mutate(has_zse=ifelse(!is.na(z_se),1,0)) %>% mutate(lacks_sd=ifelse(is.na(end_sd_con)&is.na(bas_sd_con)&is.na(bas_sd_all)&is.na(end_sd_treatment)&is.na(end_sd_all),1,0)) %>% mutate(has_streatment=ifelse(!is.na(se_treatment_effect),1,0)) 

excluded_3<-clean %>% filter(is.na(z_se))%>% select (authors, year, title, include_yn, include_why) %>% mutate(include_yn='N') %>% mutate(include_why='Not treatment sd (Z)') 
excluded_4<-clean %>% filter(is.na(z_treatment_effect)&index_with_components==1)%>% select (authors, year, title, include_yn, include_why) %>% mutate(include_yn='N') %>% mutate(include_why='Not treatment effect (Z)') 


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


########### Select variables and filter ################################################################################################
########################################################################################################################################


# Filter obs with standarized treatment effect 

incomplete <- clean %>% filter(is.na(z_treatment_effect))

clean<-clean %>% filter(!is.na(z_treatment_effect))

# Select variables

final<-clean %>% 
  filter(include_yn!='N' & !is.na(z_treatment_effect)&index_with_components!=1) %>%
  select(authors, include_yn,  include_why, title, year, baseline_year, country, treatment_type, duration_month, 
         z_treatment_effect, intervention_value, all_n,treatment_n, control_n, se_treatment_effect, population, depression_yn, 
         outcome, outcome_negative,treatment_type,transfer_type, outcome_category, archive_name, 
         int_Asset,            
         int_CCT, int_Grad,int_Healthcare, int_Lottery, int_Neighborhood,
         int_Savings, int_UCT, treatment=int, age, age_range, female_share, authors, duration, population,z_se, LMIC,  lumpsum) 


########### Creating and transforming variables for analysis  ################################################################################################
########################################################################################################################################


final<-final %>% mutate(term=case_when(duration_month<=3 ~ 'less than 3 months',
                                       duration_month>3&duration_month<=12 ~ '3-12 months',
                                       duration_month>3&duration_month>12&duration_month<=24 ~ '12-24 months',
                                       duration_month>3&duration_month>24 ~ '+24 months'))

final<-final %>% mutate(female_share=as.character(female_share)) %>%
  mutate(female_share=str_replace_all(female_share, "%", "")) %>%
  mutate(female_share=as.numeric(female_share))  %>%
  mutate(female_share = female_share/100) %>%
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


final<-final %>% filter(outcome_category!='None') %>%
  filter(z_treatment_effect<=1) %>%
  filter(z_treatment_effect>=-1) 

excluded<- rbind(excluded_1, excluded_2, excluded_3, excluded_4) %>% mutate(include_why=as.character(include_why)) %>% unique()

excluded<- aggregate(. ~ authors+year+title+include_yn, data = excluded, toString)

included<- final %>% select (authors, year, title, include_yn, include_why) %>%  mutate(include_yn='Y') %>% mutate(include_why="") %>% unique()

excluded <- excluded %>% mutate(authors=as.character(authors)) %>% mutate(title=as.character(title))
included <- included %>% mutate(authors=as.character(authors)) %>% mutate(title=as.character(title))

list<-rbind(excluded, included)
list<- mutate(list, include_yn=as.character(include_yn))
#excluded<- mutate(excluded, include_yn=as.character(include_yn))
list<- aggregate(. ~ authors+year+title, data = list, toString)
#list<- aggregate(. ~ authors+year+title, data = excluded, toString)
list <- list %>% mutate(include_yn=ifelse(include_yn!='Y'&include_yn!='N', 'Y', include_yn)) %>% mutate(include_why=ifelse(include_yn=='Y', NA, include_why))
list<-list %>% filter(include_yn!='Y')
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

setwd(diroverleaf)
list.tex <- xtable(list,
                   align=c("p{0.1cm}",
                           "p{2cm}", #columnn study
                           "p{1cm}",
                           "p{8cm}", #ntervention
                           "p{1.2cm}", #cost
                           "p{2.5cm}"),
                       caption="Studies excluded")


print(list.tex, file = "tables/list.tex", compress = FALSE, 
      include.rownames=FALSE,
      hline.after = rep(c(-1:nrow(list)),1), size = "scriptsize",
      caption.placement =  "top",
      tabular.environment="longtable")

##  included in regressions with covariates



covariates_1<- final %>% select (authors, year, title, include_yn, include_why, age ) %>%  mutate(include_yn='Y') %>% mutate(include_why="") %>% unique()

covariates_1<-aggregate(.~ authors+year+title+include_yn, data=covariates_1, function(x) {sum(!is.na(x))}, na.action = NULL) %>% filter(age==0) %>% select(-age) %>% mutate(include_yn='N') %>% mutate(include_why='missing age')

covariates_2<- final %>% select (authors, year, title, include_yn, include_why, female_share ) %>%  mutate(include_yn='Y') %>% mutate(include_why="") %>% unique()

covariates_2<-aggregate(.~ authors+year+title+include_yn, data=covariates_2, function(x) {sum(!is.na(x))}, na.action = NULL) %>% filter(female_share==0) %>% select(-female_share) %>% mutate(include_yn='N') %>% mutate(include_why='missing female_share')


covariates_3<- final %>% select (authors, year, title, include_yn, include_why, duration_years ) %>%  mutate(include_yn='Y') %>% mutate(include_why="") %>% unique()

covariates_3<-aggregate(.~ authors+year+title+include_yn, data=covariates_3, function(x) {sum(!is.na(x))}, na.action = NULL) %>% filter(duration_years==0) %>% select(-duration_years) %>% mutate(include_yn='N') %>% mutate(include_why='missing duration_years')

covariates<-rbind(covariates_1, covariates_2)

covariates <- covariates %>%  mutate(include_yn=as.character(include_yn)) %>%  mutate(include_why=as.character(include_why))
covariates<- aggregate(. ~ authors+year+title, data = covariates, toString)


covariates.tex <- xtable(covariates,
                   align=c("p{0.1cm}",
                           "p{2cm}", #columnn study
                           "p{1cm}",
                           "p{8cm}", #ntervention
                           "p{1.2cm}", #cost
                           "p{2.5cm}"),
                   caption="Studies excluded")


print(covariates.tex, file = "tables/covariates.tex", compress = FALSE, 
      include.rownames=FALSE,
      hline.after = rep(c(-1:nrow(covariates)),1), size = "scriptsize",
      caption.placement =  "top",
      tabular.environment="longtable")

setwd(dir)