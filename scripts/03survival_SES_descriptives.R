##Survival among people with MLTCs by deprivation and age
#Decriptives for survival analysis

# Setup -------------------------------------------------------------------

#Load library
library(plyr)
library(tidyverse)
library(tableone)
library(ggplot2)
library(broom)
library(survival)
library(survminer)
library(gtsummary)
library(stringr)

#Load data
all_data2 <- readRDS(str_c(outputs_path, 'all_data2.rds'))

# Descriptive ------------------------------------------------------------

## Describe follow-up time and events by IMD and multimorbidity status
vars <- c("years_in_study_cprd", "yearsMM", "event", "total", "EndMLTC_count")
catvars <- c("event")
TableStrat <- CreateTableOne(vars=vars, strata=c("PHMHLTC"), data=all_data2, factorVars = catvars)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'anne_test1.csv')))

vars <- c("years_in_study_cprd", "yearsMM", "event", "total", "EndMLTC_count")
catvars <- c("event")
TableStrat <- CreateTableOne(vars=vars, strata=c("imd2015_10"), data=all_data2, factorVars = catvars)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'anne_test2.csv')))

vars <- c("years_in_study_cprd", "event", "total_ca", "age_baseline3", "gender", "imd2015_10")
catvars <- c("event")
TableStrat <- CreateTableOne(vars=vars, data=all_data2, factorVars = catvars)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'anne_test3.csv')))

vars <- c("years_in_study_cprd", "event", "total_ca", "age_baseline3", "gender")
TableStrat <- CreateTableOne(vars=vars, strata=c("imd2015_10"), data=all_data2, factorVars = catvars)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'anne_test4.csv')))


tab<-all_data2 %>% 
  select(age_baseline3, gender, imd2015_10,event) %>% 
  tbl_summary(digits=list(everything() ~ 1)) %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

write.csv(tab, file=(str_c(outputs_path, 'anne_test4x.csv')))

tab2<-all_data2 %>% 
  select(age_baseline3, gender, imd2015_10,event) %>% 
  tbl_summary(by=imd2015_10, digits=list(everything() ~ 1)) %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

write.csv(tab2, file=(str_c(outputs_path, 'anne_test4y.csv')))


#Number of LTcs at end of follow up 
all_data2_patsc <- ddply(all_data2, c("PHMHLTC", "imd2015_10"), summarise,
                          N=length(EndMLTC_count),
                          mean=mean(EndMLTC_count),
                          sd=sd(EndMLTC_count),
                          ci=1.96*sd/sqrt(N))

pd <- position_dodge(0.2)
ggplot(all_data2_patsc, aes(x=PHMHLTC, y=mean, colour=imd2015_10)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=0.1, position=pd) +
  geom_point(position=pd) + 
  ylab("Mean number of LTCs at end of follow-up")+
  xlab("Multimorbidity (MM) type")+
  scale_y_continuous(limits = c(0,4))+
  theme(legend.text = element_text(size=12),legend.position="bottom", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
ggsave(file=(str_c(outputs_path, 'graph2.png')), dpi = 300, width = 6.5, height = 6.5)


IMDnonmiss_patsc<-IMDnonmiss_patsc %>% 
  mutate(lwr=mean-ci, 
         uper=mean+ci)

##Number of LTCs at baseline  
all_data2_patsc2 <- ddply(all_data2, c("PHMHLTC", "imd2015_10"), summarise,
                           N=length(total),
                           mean=mean(total),
                           sd=sd(total),
                           ci=1.96*sd/sqrt(N))
pd <- position_dodge(0.2)
ggplot(all_data2_patsc2, aes(x=PHMHLTC, y=mean, colour=imd2015_10)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=0.1, position=pd) +
  geom_point(position=pd) + 
  ylab("Mean number of LTCs at baseline")+
  xlab("Multimorbidity (MM) type")+
  scale_y_continuous(limits = c(0,4))+
  theme(legend.text = element_text(size=12),legend.position="bottom", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
ggsave(file=(str_c(outputs_path, 'graph.png')), dpi = 300, width = 6.5, height = 6.5)


#checking correlation between variables
var<-all_data2 %>% 
  mutate(gender_num=as.numeric(gender)) %>% 
  dplyr::select(imd2015_10_num, startage, total, gender_num)


cor_output<-cor(var)
write.csv(cor_output, file=(str_c(outputs_path, 'anne_test5.csv')))

#Prevalence of conditions

prevalence<-all_data2 %>% 
  select(c("alcohol","arthritis","asthma","atrial_fibrillation", "cancer","copd","dementia","diabetes",
            "epilepsy","heart_disease","heart_failure","hypertension","kidney_disease","multiple_sclerosis",
            "parkinsons","psoriasis","schizophrenia","stroke","thyroid_disorders","anxiety_depression", "blindness",
            "bronchiectasis","diverticulosis","hearing_loss","liver_disease","substance_misuse")) %>% 
  tbl_summary() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

write.csv(prevalence, file=(str_c(outputs_path, 'anne_test6.csv')))


#Descriptive for age, sex, imd and event 


tab<-all_data2 %>% 
  select(age_baseline3, gender, imd2015_10,event) %>% 
  tbl_summary() %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

write.csv(tab, file=(str_c(outputs_path, 'anne_test4x.csv')))

#Descriptive for censored 

all_data2<-all_data2 %>% 
  mutate(lost_follow_up=case_when(event==2~ "died", 
                                  censoring_date_cprd=="2019-12-31" | years_in_study_cprd==5 ~ "complete",
                                  censoring_date_cprd!="2019-12-31" ~ "censored")) %>% 
  mutate(years_follow_up=case_when(event==2~ "died", 
                                   years_in_study_cprd==5~ "complete", 
                                   years_in_study_cprd<5 & years_in_study_cprd>=4 ~ "4+ years", 
                                   years_in_study_cprd<4 & years_in_study_cprd>=3 ~ "3+ years", 
                                   years_in_study_cprd<3 & years_in_study_cprd>=2 ~ "2+ years", 
                                   years_in_study_cprd<2 & years_in_study_cprd>=1 ~ "1+ year", 
                                   years_in_study_cprd<1  ~ "< 1 year"))



tab2<-all_data2 %>% 
  select(lost_follow_up,years_follow_up, years_in_study_cprd) %>% 
  tbl_summary(statistic=list(all_continuous()~ "{mean} ({sd})"), digits= everything()~ 1) %>% 
  as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


write.csv(tab2, "days_and_years_follow_up.csv")

