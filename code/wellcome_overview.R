


#dir<-"/Users/haushofer/Dropbox/wellcome_review_2020/analysis/data/processed"
#outdir<-"/Users/haushofer/Dropbox/wellcome_review_2020/analysis/code/tables/output"


setwd(dir)

load(file='data/final/final.Rdata')
overview<-final %>% select(archive_name, title, age_range, authors, year,treatment , duration, country, intervention_value, outcome_category, z_treatment_effect) %>%
  mutate_at(c('title','authors', 'country', 'duration', 'age_range'),as.character) %>%
  filter(!is.na(z_treatment_effect)) %>% select(-z_treatment_effect) 
final<- final  %>% mutate(treatment=ifelse(treatment =='Neighborhood', 'Housing Voucher', 
                                                                            ifelse(treatment =='Healthcare', 'Insurance Provision', 
                                                                                   ifelse(treatment=='Asset','Asset Transfer',
                                                                                          ifelse(treatment=='Grad','Poverty Graduation Program', treatment)))))


# intervention
part1<- final %>% select(archive_name, authors, year,title,treatment) %>% unique()
part1<- aggregate(. ~ archive_name+authors+year+title, data = part1, toString)

# duration
part2<- final %>% select(archive_name, authors, year,title,duration)  %>% mutate_at('duration', as.character)%>% unique()
part2<- aggregate(. ~ archive_name+authors+year+title, data = part2, toString)

# country
part3<- final %>% select(archive_name, authors, year,title,country) %>% mutate_at('country', as.character) %>% unique()
part3<- aggregate(. ~ archive_name+authors+year+title, data = part3, first)

part3<-part3 %>% mutate(country=ifelse(archive_name=='Banerjee_15','Ethiopia, Ghana, Honduras, India, Pakistan, Peru', country))

# intervention value
#part4<- final %>% select(archive_name, authors, year,title,intervention_value) %>% 
#  mutate(intervention_value=round(intervention_value,digits=0))%>%
 # mutate_at('intervention_value', as.character) %>% unique()
#part4<- aggregate(. ~ archive_name+authors+year+title, data = part4, toString)

part4<- final %>% select(archive_name, authors, year,title,intervention_value) %>% 
    mutate(intervention_value=round(intervention_value,digits=0))%>%
    mutate_at('intervention_value', as.numeric) %>% unique()
part4<- aggregate(. ~ archive_name+authors+year+title, data = part4, max)
part4<-part4 %>%mutate(intervention_value=round(intervention_value, 0)) %>% mutate_at('intervention_value', as.character)

# outcome
part5<- final %>% select(archive_name, authors, year,title,outcome_category) %>% mutate_at('outcome_category', capitalize)  %>% unique()
part5<- aggregate(. ~ archive_name+authors+year+title, data = part5, toString)

# population 
part6<- final %>% select(archive_name, authors, year,title, target) %>%
  mutate_at('target', as.character) %>% mutate_at('target', capitalize) %>% unique()
#part6<- aggregate(. ~ archive_name+authors+year+title, data = part6, toString)
part6<- aggregate(. ~ archive_name+authors+year+title, data = part6, first)


# sample size 

part7<- final %>% select(archive_name, authors, year,title,all_n) %>% unique()
part7<- aggregate(. ~ archive_name+authors+year+title, data = part7, FUN=max) 

part7 %>%mutate(all_n=round(all_n, 0))%>% mutate_at('all_n', as.character)
# 
# # age_range
# part8<- final %>% select(archive_name, authors, year,title,age_range) %>%  mutate_at('age_range', as.character) %>% unique()
# part8<- aggregate(. ~ archive_name+authors+year+title, data = part8, toString)

# %>% mutate_at('age_range', capitalize) %>% unique()

overview<-merge(part1, part4, by=c('archive_name', 'authors', 'year','title'),all=TRUE)
overview<-merge(overview, part5, by=c('archive_name', 'authors', 'year','title'),all=TRUE)
overview<-merge(overview, part2, by=c('archive_name', 'authors', 'year','title'),all=TRUE)
overview<-merge(overview, part6, by=c('archive_name', 'authors', 'year','title'),all=TRUE)
#overview<-merge(overview, part8, by=c('archive_name', 'authors', 'year','title'),all=TRUE)
overview<-merge(overview, part3, by=c('archive_name', 'authors', 'year','title'),all=TRUE)
overview<-merge(overview, part7, by=c('archive_name', 'authors', 'year','title'),all=TRUE)


overview<-overview  %>%
  mutate(authors = paste(as.character(authors), as.character(year), sep=", ")) %>% 
  select(-c('archive_name','year','title')) %>%mutate(all_n=round(all_n, 0))%>% mutate_at('all_n', as.character)

overview<-overview[order(overview$treatment),]
save(overview, file='./data/final/overview.Rdata')
names(overview)<-c('Study',"Intervention type","Average transfer value (USD)","Outcomes", "Delay Intervention--Survey",
                   "Target \n population", "Country", "Sample \n size")


x <- c("UCT","UCT, CCT","UCT, Insurance Provision", "Housing Voucher", "CCT","Lottery","Poverty Graduation Program","Asset Transfer","Insurance Provision")

overview<-overview %>% arrange(sapply(`Intervention type`, function(y) which(y == x)))


bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}


overview_1<-overview[1:15,]
overview_2<-overview[16:nrow(overview),]


overview.tex <- xtable(overview, 
                         align=c("p{0.1cm}",
                                 "p{1.8cm}", #columnn study
                                 "p{1.7cm}", #ntervention
                                 "p{1.3 cm}", #cost
                                 "p{1.8cm}", #outcomes
                                 "p{1.7cm}", #delay
                                 "p{2cm}", #target pop
                                 "p{1.2cm}",
                                 "p{1cm}"),
                         caption="Study Overview",
                       lable='studyoverview')
# 
# 
# overview_1.tex <- xtable(overview_1, 
#                        align=c("p{0.1cm}",
#                         "p{1.8cm}", #columnn study
#                                "p{1.7cm}", #ntervention
#                                "p{1.3 cm}", #cost
#                                "p{2cm}", #outcomes
#                                "p{1.7cm}", #delay
#                                "p{2.0cm}", #target pop
#                                "p{1cm}", #age range
#                                "p{1.2cm}",
#                         "p{1cm}"),
#                        caption="Study Overview")
# 
# 
# overview_2.tex <- xtable(overview_2, 
#                          align=c("p{0.1cm}",
#                                  "p{1.8cm}", #columnn study
#                                  "p{1.7cm}", #ntervention
#                                  "p{1.3 cm}", #cost
#                                  "p{2cm}", #outcomes
#                                  "p{1.7cm}", #delay
#                                  "p{2.0cm}", #target pop
#                                  "p{1cm}", #age range
#                                  "p{1.2cm}",
#                                  "p{1cm}"))

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  "{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))

setwd(diroverleaf)

print(overview.tex, file = "tables/overview.tex", compress = FALSE,
      include.rownames=FALSE,
      add.to.row = addtorow,   
      hline.after = rep(c(-1:nrow(overview)),1),
      sanitize.colnames.function=bold, size = "scriptsize",
      caption.placement =  "top",
      tabular.environment="longtable", 
      floating=FALSE)
# 
# print(overview.tex, file = "tables/overview.tex", compress = FALSE, 
#       include.rownames=FALSE,
#       hline.after = rep(c(-1:nrow(overview)),1),
#       sanitize.colnames.function=bold, size = "scriptsize",
#       caption.placement =  "top",
#       include.colnames=FALSE, 
#       floating=FALSE)


# In latex paste before tabular: 
#\scriptsize
#\begin{tabular}{|p{2.8cm}|p{1.7cm}|p{0.75cm}|p{4cm}|p{1.4cm}|p{2.3cm}|p{1.25cm}|p{0.85cm}|}