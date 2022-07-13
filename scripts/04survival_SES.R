#Final survival analysis included in the paper 

# Setup -------------------------------------------------------------------

#Load library
library(dplyr)
library(plyr)
library(tableone)
library(ggplot2)
library(survival)
library(survminer)
library(rms)
library(broom)
library(RVAideMemoire)
library(stringr)
library(freqtables)
library(gtsummary)
library(tibble )

#Functions

fit_test<-function(x=m1, z=m2) {
  a<-anova(x, z)
  print(a)
  b<-AIC(x, z)
  print(b)
  c<-BIC(x, z)
  print(c)
  d<-concordance(x,z)
  print(d)
}


diag_test<-function(model=m1) {
  a<-cox.zph(model)  
  print(a)
  b<-ggcoxzph(a)
  print(b)
}


index.plot<- function (var.x=total, var.y=resid_m1){
  var.x<-enquo(var.x)
  var.y<-enquo(var.y)
  ggplot(data=plot.data, mapping=aes(x=!!var.x, y=!!var.y))+
    geom_point()+
    geom_smooth()+
    theme_bw()+theme(legend.key = element_blank())
}
##smoothing method is gam; cannot use loess or lowess because of the large sample size



# Load data ---------------------------------------------------------------

all_data2 <- readRDS(str_c(processed_RDS_path, 'all_data2.rds'))



# is number of LTCs linear -----------------------------------------------------

plot.data<-all_data2

#Residuals for null model 
m_null<-coxph(Surv(years_in_study_cprd, event) ~ 1, data=all_data2)
plot.data$resid_m1<-resid(m_null, type="martingale", collapse = all_data2$patid)

#plot against residuals
index.plot(var.x=total_ca)

#need to cap total number of conditions to preserve linearity 

all_data2<-all_data2 %>% 
  mutate(total_ca_cap=ifelse(total_ca>6,6,total_ca))

plot.data<-all_data2

plot.data$resid_m1<-resid(m_null, type="martingale", collapse = all_data2$patid)

ggplot(data=plot.data, mapping=aes(x=total_ca_cap, y=resid_m1))+
    geom_point()+
    geom_smooth(method="gam", formula=y ~ s(x, bs = "cs", k=6))+
    theme_bw()+theme(legend.key = element_blank())


all_data2<-all_data2 %>% 
  mutate(total_ca_cat=as.factor(total_ca_cap))

coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + total_ca_cat , data=all_data2)
coxoutput

coxoutput %>% 
  tidy(conf.int=TRUE, exponetiate=TRUE) %>% 
  write.csv(., file=(str_c(outputs_path, 'anne_suppltable3.csv')))

write.csv(glance(coxoutput), file=(str_c(outputs_path, 'anne_suppltable3fit.csv')))



# Counts of conditions to compare with Ontario --------------------------------------------------

counts_conds<-all_data2 %>% 
  select(total_ca_cap) %>% 
  tbl_summary(type=everything()~"categorical") %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

write.csv(counts_conds, file=(str_c(outputs_path, 'anne_test4x.csv')))



# Frequencies-check counts of  events by deprivation and conditions (using Ontario total) -------------------------------------------------------------------
#check if there is atleast 10 events per covariate or per group split 

all_data2 %>% 
  tbl_summary(by=event, include=c(imd2015_10, total_ca_cap, event), 
              type=everything()~"categorical")

all_data2 %>% 
  filter(event==2) %>% 
  tbl_summary(by=age_baseline3, include=c(age_baseline3, imd2015_10, total_ca_cap), 
              type=everything()~"categorical")



# Model testing -----------------------------------------------------------


m_h1<-coxph(Surv(years_in_study_cprd, event) ~ age_baseline+gender+imd2015_10+ cluster(pracid), data=all_data2)
summary(m_h1)


m_h1 %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table1.csv')))

write.csv(glance(m_h1), file=(str_c(outputs_path, 'anne_table1fit.csv')))

diag_test(m_h1)

ggsurvplot(survfit(m_h1), data=all_data2,
            xlab="Years",
            ylad="Overall survival probability",
           ylim=c(0.90,1))

#H2: The association between SE deprivation and mortality is stronger/larger in those <65 vs 65+ 

m_h2<-coxph(Surv(years_in_study_cprd, event) ~ age_baseline*imd2015_10+gender+ cluster(pracid), data=all_data2)
summary(m_h2)

m_h2 %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table2.csv')))

write.csv(glance(m_h2), file=(str_c(outputs_path, 'anne_table2fit.csv')))

fit_test(m_h1, m_h2)

all_data2_younger<-all_data2 %>% 
  filter(age_baseline<65)

all_data2_older<-all_data2 %>% 
  filter(age_baseline>64)

m_h2_younger<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10+gender+age_baseline+ cluster(pracid), data=all_data2_younger)
summary(m_h2_younger)

m_h2_older<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10+gender+age_baseline+ cluster(pracid), data=all_data2_older)
summary(m_h2_older)


#H3: does MLTCs contribute to explaining some of these inequalities in the age groups 

m_h3_younger<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10+gender+age_baseline+total_ca_cap+ cluster(pracid), data=all_data2_younger)
summary(m_h3_younger)

m_h3_younger %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table3a.csv')))

write.csv(glance(m_h3_younger), file=(str_c(outputs_path, 'anne_table3afit.csv')))



m_h3_older<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10+gender+age_baseline+total_ca_cap+ cluster(pracid), data=all_data2_older)
summary(m_h3_older)


m_h3_older %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table3b.csv')))

write.csv(glance(m_h3_older), file=(str_c(outputs_path, 'anne_table3bfit.csv')))


#H4: The association between MLTCs and mortality depend on level SE deprivation and this is true for those all age groups

m_h4_younger<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10*total_ca_cap+gender+age_baseline+ cluster(pracid), data=all_data2_younger)
summary(m_h4_younger)

m_h4_younger %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table4a.csv')))

write.csv(glance(m_h4_younger), file=(str_c(outputs_path, 'anne_table4afit.csv')))



m_h4_older<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10*total_ca_cap+gender+age_baseline+ cluster(pracid), data=all_data2_older)
summary(m_h4_older)


m_h4_older %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table4b.csv')))

write.csv(glance(m_h4_older), file=(str_c(outputs_path, 'anne_table4bfit.csv')))



#Model fit 
fit_test(m_h1, m_h2)

fit_test(m_h2_younger, m_h3_younger)
fit_test(m_h2_older, m_h3_older)

fit_test(m_h3_younger, m_h4_younger)
fit_test(m_h3_older,m_h4_older)


# Testing proportional hazard assumption ----------------------------------


model_test<-coxph(Surv(years_in_study_cprd, event) ~ startage+total_ca_cap+imd2015_10+gender, data=all_data2)
diag_test(model_test)

all_data3<-all_data2 %>% 
  filter(years_in_study_cprd>0) %>% 
  mutate(age_dummy=as.numeric(age_baseline3),
         gender_dummy=as.numeric(gender))


splitdata <- survSplit(Surv(years_in_study_cprd, event)  ~ startage+imd2015_10+gender_dummy+total_ca_cap,
                       data=all_data3,
                       cut=c(2.5),
                       start="tstart",
                       id="patid",
                       zero=0,
                       episode="tgroup",
                       event="event")

intervalcoxoutput <- coxph(Surv(tstart, years_in_study_cprd, event) ~imd2015_10+startage:strata(tgroup)+gender_dummy:strata(tgroup)+total_ca_cap, data=splitdata)
summary(intervalcoxoutput)
diag_test(intervalcoxoutput)


# Sensitivity analysis ----------------------------------------------------

all_data2<-all_data2 %>% 
  mutate(total_ca_full_cap=ifelse(total_ca_full>6,6,total_ca_full))

all_data2_younger<-all_data2 %>% 
  filter(age_baseline<65)

all_data2_older<-all_data2 %>% 
  filter(age_baseline>64)

#H3: does MLTCs contribute to explaining some of these inequalities in the age groups 

m_h3_younger<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10+gender+age_baseline+total_ca_full_cap+ cluster(pracid), data=all_data2_younger)
summary(m_h3_younger)


m_h3_younger %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table3a.csv')))

write.csv(glance(m_h3_younger), file=(str_c(outputs_path, 'anne_table3afit.csv')))



m_h3_older<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10+gender+age_baseline+total_ca_full_cap+ cluster(pracid), data=all_data2_older)
summary(m_h3_older)

m_h3_older %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table3b.csv')))

write.csv(glance(m_h3_older), file=(str_c(outputs_path, 'anne_table3bfit.csv')))


#H4: The association between MLTCs and mortality depend on level SE deprivation and this is true for those all age groups

m_h4_younger<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10*total_ca_full_cap+gender+age_baseline+ cluster(pracid), data=all_data2_younger)
summary(m_h4_younger)

m_h4_younger %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table4a.csv')))

write.csv(glance(m_h4_younger), file=(str_c(outputs_path, 'anne_table4afit.csv')))


m_h4_older<-coxph(Surv(years_in_study_cprd, event) ~ imd2015_10*total_ca_full_cap+gender+age_baseline+ cluster(pracid), data=all_data2_older)
summary(m_h4_older)

m_h4_older %>%
  tidy(conf.int=TRUE, exponetiate=TRUE) %>%
  write.csv(., file=(str_c(outputs_path, 'anne_table4b.csv')))

write.csv(glance(m_h4_older), file=(str_c(outputs_path, 'anne_table4bfit.csv')))


#fit test
fit_test(m_h1, m_h2)

fit_test(m_h2_younger, m_h3_younger)
fit_test(m_h2_older, m_h3_older)

fit_test(m_h3_younger, m_h4_younger)
fit_test(m_h3_older,m_h4_older)




