##Survival by deprivation of people with MLTCs
#Data prep for survival analysis



# Set up ------------------------------------------------------------------

#Load library
library(tidyverse)
library(plyr)
library(tableone)
library(ggplot2)
library(gtsummary)
library(stringr)

#Load data 

all_data1 <- readRDS(str_c(processed_RDS_path, 'all_data1.rds'))

# Adding Complex MM and age-----------------------------------------
all_data1 <- all_data1 %>% 
  mutate(bodysystem1=ifelse(cancer==1, 1, 0)) %>% #cancer
  mutate(bodysystem2=ifelse(atrial_fibrillation==1 | heart_disease==1 | heart_failure==1 | hypertension==1 | stroke==1 | vascular_disease==1, 1, 0)) %>% #circulatory system    mutate(bodysystem3=ifelse(liver_disease==1 | diverticulosis==1 | ibs==1, 1, 0)) %>%
  mutate(bodysystem4=ifelse(hearing_loss==1, 1, 0)) %>% #diseases of the ear 
  mutate(bodysystem5=ifelse(diabetes==1 | kidney_disease==1 | thyroid_disorders==1, 1, 0)) %>% #endocrine system
  mutate(bodysystem6=ifelse(blindness==1, 1, 0)) %>% #diseases of the eye 
  mutate(bodysystem7=ifelse(kidney_disease==1, 1, 0)) %>% #genitourinary system 
  mutate(bodysystem8=ifelse(asthma==1 | bronchiectasis==1 | copd==1, 1, 0)) %>% #respiratory system 
  mutate(bodysystem10=ifelse(viral_hepatitis==1, 1, 0)) %>% #infectious disease
  mutate(bodysystem11=ifelse(alcohol==1 | anxiety_depression==1 | schizophrenia==1 | dementia==1 | anorexia==1 | learning_disability==1 | substance_misuse==1, 1, 0)) %>% #mental health disorders
  mutate(bodysystem12=ifelse(arthritis==1, 1, 0)) %>% #musculoskeletal conditions
  mutate(bodysystem13=ifelse(epilepsy==1 | migraine==1 | multiple_sclerosis==1 | parkinsons==1, 1, 0)) %>% #neurological conditions
  mutate(bodysystem15=ifelse(psoriasis==1, 1, 0)) %>%  #skin conditions
  mutate(totbodysystems=bodysystem1 + bodysystem2 + bodysystem4 + bodysystem5 + bodysystem6 + bodysystem7 + bodysystem8 + bodysystem10 + bodysystem11 + bodysystem12 + bodysystem13 + bodysystem15) %>%
  mutate(complexMM=ifelse(totbodysystems>2, 1, 0)) %>% 
  mutate(age_baseline=2015-yob) %>% ## derive age at baseline
  mutate(age_baseline_centredat50=2015-yob-50) %>%
  mutate(age_baseline_centredsq=age_baseline_centredat50*age_baseline_centredat50) %>% 
  mutate(age_baseline3=factor(case_when(age_baseline<65 ~ "Under 65", 
                                        age_baseline>64 ~ "65+"), levels=c("Under 65", "65+")))
  
all_data1$complexMM <- factor(all_data1$complexMM, labels=c("Not MM", "complex MM"))




# Checking data with missing age, sex, and deprivation -----------------------------------------------------------------

excluded_data<-all_data1 %>% 
  filter(is.na(age_baseline) | is.na(gender) | is.na(imd2015_10)) 

excluded_data<-excluded_data %>% 
  mutate(total_ca=alcohol+arthritis+asthma+atrial_fibrillation+cancer+copd+dementia+diabetes+
           epilepsy+heart_disease+heart_failure+hypertension+
           kidney_disease+multiple_sclerosis+parkinsons+psoriasis+schizophrenia+stroke+thyroid_disorders, 
         total_ca_full=total_ca+anxiety_depression+blindness+bronchiectasis+diverticulosis+
           hearing_loss+liver_disease+substance_misuse)


vars <- c("age_baseline1", "gender", "yearsMM", "event", "total_ca")
catvars <- c("event")
TableStrat <- CreateTableOne(vars=vars, data=excluded_data, factorVars = catvars)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)


write.csv(outputTable, file=(str_c(outputs_path, 'anne_test1_missing.csv')))

tab<-excluded_data %>% 
  select(age_baseline3) %>% 
  tbl_summary() %>% 
  bold_labels() %>% 
  as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

write.csv(tab, file=(str_c(outputs_path, 'anne_test2_missing.csv')))

# Extracting analytical sample (no missing), changing deprivation into a factor ---------------------------------------
all_data2 <- all_data1 %>%
  filter(!is.na(age_baseline) & !is.na(gender) & !is.na(imd2015_10)) %>% 
     mutate(imd2015_10=factor(imd2015_10, levels=c(1:10), labels=c(1:10)))


# Adding time variables and years MM ---------------------------------------------------

all_data2 <- all_data2 %>%
  group_by(patid) %>%
  mutate(study_end=c("31/12/2019")) %>%
  mutate(study_end=as.Date(study_end,"%d/%m/%Y")) %>%
  mutate(regenddate=as.Date(regenddate,"%Y-%m-%d")) %>%
  mutate(ddate=as.Date(ddate,"%Y-%m-%d")) %>%
  mutate(lcd=as.Date(lcd,"%d/%m/%Y")) %>%
  mutate(censoring_date_cprd=min(regenddate, lcd, study_end, na.rm=TRUE)) %>%
  mutate(days_in_study_cprd=(as.numeric(censoring_date_cprd - as.Date('2015-01-01')))) %>%
  mutate(years_in_study_cprd=round(as.numeric(censoring_date_cprd - as.Date('2015-01-01'))/365, 2)) %>%
  mutate(years_in_study_cprd=as.numeric(years_in_study_cprd)) %>%
  mutate(event=ifelse(years_in_study_cprd<5 & !is.na(ddate), 2, 1)) %>% ## event is 2 if died and 1 otherwise
  ungroup()

all_data2 <- all_data2 %>%
  mutate(yearsMM=(as.numeric(difftime(as.Date('2015-01-01'), oldest_cond, units="weeks")))/52.25) %>%
  mutate(yearsMM=ifelse(total<2, 0, yearsMM))



#  Adding additional variables to aid interpretation of models ------------

all_data2 <- all_data2 %>%
  mutate(MM=ifelse(total>1, 2, total)) %>%
  mutate(startage=2015-yob-50) %>% ## derive age at baseline and to aid interpretation
  mutate(startagesq=startage*startage) %>% 
  mutate(imd2015_10_num=as.numeric(imd2015_10)) %>%  #to try a linear relationship with IMD
  mutate(totalcat=ifelse(total>8, 8, total)) %>% 
  mutate(totalcat=factor(totalcat)) #to try total number of conditions as categories


# Adding total conditions to match Ontario --------------------------------

all_data2<-all_data2 %>% 
mutate(total_ca=alcohol+arthritis+asthma+atrial_fibrillation+cancer+copd+dementia+diabetes+
         epilepsy+heart_disease+heart_failure+hypertension+
         kidney_disease+multiple_sclerosis+parkinsons+psoriasis+schizophrenia+stroke+thyroid_disorders, 
       total_ca_full=total_ca+anxiety_depression+blindness+bronchiectasis+diverticulosis+
         hearing_loss+liver_disease+substance_misuse)


#save data set for analysis 

saveRDS(all_data2, file=(str_c(outputs_path, 'all_data2.rds')))







