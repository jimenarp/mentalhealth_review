setwd(dir)

load(file='./data/final/final.Rdata')

# getting one effect per group 
final<-final %>% mutate(one_outcome=NA) %>% mutate(one_outcome_sd=NA) %>%  filter(!is.na(z_treatment_effect))  %>% filter(treatment !='Savings')

ids<-unique(final$id)


for (i in 1: length(ids)){
  tmp<-ids[[i]]
  tmpdata <- final[which(final$id==tmp),]
  if (nrow(tmpdata)==1) {
    one_trea=tmpdata$z_treatment_effect
    one_sd=tmpdata$z_se
  } else{
    temp_reg<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=tmpdata, method='SJ')
    one_trea<-temp_reg$beta[1,1]
    one_sd<-temp_reg$se
  }
  final[which(final$id==tmp),]$one_outcome=one_trea
  final[which(final$id==tmp),]$one_outcome_sd=one_sd
  print(one_trea)
}


final_one<- final %>% select(c("title","year","baseline_year","id","one_outcome" , "one_outcome_sd","archive_name", "authors", 
                               "year")) %>%
  mutate(one_outcome=as.numeric(as.character(one_outcome))) %>%
  mutate(one_outcome_sd=as.numeric(as.character(one_outcome_sd))) %>% 
  mutate(authors = paste(as.character(authors), as.character(year), sep=", ")) 

final_one<-aggregate(.~id,final_one, FUN='first')

final_one<-final_one %>%
  mutate(one_outcome=as.numeric(as.character(one_outcome))) %>%
  mutate(one_outcome_sd=as.numeric(as.character(one_outcome_sd)))
a<-final_one %>% filter(id=='Leventhal & Dupere 2011')


save(final_one, file='./data/final/final_one.Rdata')

