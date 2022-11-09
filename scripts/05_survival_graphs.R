##Survival among people with MLTCs by deprivation and age in England and Canada

#Graphs used in the paper

# Setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)
library(janitor)
library(tidyverse)
library(THFstyle)
library(here)

# functions ---------------------------------------------------------------

hr.deprivation<- function (df=canada_working){
  w.env <- new.env()
  w.env$df <- df
  w.env$df<-w.env$df %>% 
    remove_empty(c("rows", "cols")) %>% 
    clean_names() %>% 
    mutate(term=ifelse(is.na(x1), t,x1)) %>% 
    select(c(term,estimate=coef,lower_ci=p_z,upper_ci=x95_percent)) %>% 
    mutate(hr=exp(estimate), hr_lower=exp(lower_ci), hr_upper=exp(upper_ci)) %>% 
    filter(!(is.na(estimate)))
  assign(deparse(substitute(df)),w.env$df, envir=.GlobalEnv)
}

txt.clean<-function(df=canada_all){
  w.env <- new.env()
  w.env$df <- df
names(w.env$df)[1]<-"col"
  w.env$df<-w.env$df %>% 
    filter(!str_detect(col,"-"), !str_detect(col, "dx")) %>% 
    mutate(col=gsub(" ",";",col)) %>%
    separate(col,c("cons", "freq", "percent","cum"),"[0-9];", extra="merge", fill="right") %>% 
    mutate_all(~gsub(";","",.)) %>% 
    mutate(cons=c(0:6)) %>% 
    mutate(freq=substr(freq,2,str_length(freq))) %>% 
    mutate(freq=gsub(",","",freq)) %>% 
    select(-cum)

  w.env$df[1:3]<-lapply(w.env$df[1:3], FUN=function(y){as.numeric(y)})
  
  assign(deparse(substitute(df)),w.env$df, envir=.GlobalEnv)
  
}

colfun<-colorRampPalette(c('#dd0031','#53a9cd'))
cc<-colfun(10)

# Figure 1- Deprivation graph -------------------------------------------------------


# load data ---------------------------------------------------------------

canada_path<-here('data/')
england_path<-here('data/')
outputs_path<-here('outputs/')

canada_working<-read_excel(str_c(canada_path, 'CA_results_models.xlsx'),sheet='Model 2a', skip=7)

canada_older<-read_excel(str_c(canada_path, 'CA_results_models.xlsx'),sheet='Model 2b', skip=8)

england_working<-read.csv(str_c(england_path, 'anne_table2a.csv'))

england_older<-read.csv(str_c(england_path, 'anne_table2b.csv'))

# Clean data --------------------------------------------------------------


hr.deprivation(df=canada_working)

hr.deprivation(df=canada_older)

england_working<- england_working %>% 
  select(c(term,estimate,lower_ci=conf.low,upper_ci=conf.high)) %>% 
  mutate(hr=exp(estimate), hr_lower=exp(lower_ci), hr_upper=exp(upper_ci))

england_older<- england_older %>% 
  select(c(term,estimate,lower_ci=conf.low,upper_ci=conf.high)) %>% 
  mutate(hr=exp(estimate), hr_lower=exp(lower_ci), hr_upper=exp(upper_ci))
  


# Combining data ----------------------------------------------------------------


canada_working<-canada_working %>% 
  filter_all(any_vars(str_detect(., 'D'))) %>% 
  mutate(deprivation=c(1:10), age_group=rep("Working age (18-64 years)",10), country=rep("Canada (Ontario)",10)) 

canada_older<-canada_older %>% 
  filter_all(any_vars(str_detect(., 'D'))) %>% 
  mutate(deprivation=c(1:10), age_group=rep("Older age (65+ years)",10), country=rep("Canada (Ontario)",10))

england_working<-england_working %>% 
  filter_all(any_vars(str_detect(., 'imd2015'))) %>% 
  mutate(deprivation=c(1:10), age_group=rep("Working age (18-64 years)",10), country=rep("England",10))

england_older<-england_older %>% 
  filter_all(any_vars(str_detect(., 'imd2015'))) %>% 
  mutate(deprivation=c(1:10), age_group=rep("Older age (65+ years)",10), country=rep("England",10))


df<-rbind(canada_working, canada_older, england_working, england_older)



# Visualising data --------------------------------------------------------

# df %>% 
#   select(c(country,age_group, deprivation, hr, hr_lower, hr_upper)) %>% 
#   ggplot(., aes(x=as.factor(deprivation), y=hr, group=age_group, colour=age_group))+
#   geom_line()+
#   geom_point()+
#   scale_y_continuous(trans='log2')+
#   facet_grid(cols = vars(country))+
#   theme_THF()+
#   scale_fill_THF()+
#   scale_colour_THF()+
#   labs(x= "Deprivation", y="Mortality Hazard Ratio", title="")+
#   theme(plot.title = element_text(size=14),
#         legend.text=element_text(size=14), legend.position="bottom", 
#         axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
#         strip.text=element_text(size=12), axis.line.x = element_line(colour='grey20'))

# 
# df %>% 
#   select(c(country,age_group, deprivation, hr, hr_lower, hr_upper)) %>% 
#   ggplot(., aes(x=as.factor(deprivation), y=hr, group=age_group, colour=age_group))+
#   geom_line()+
#   geom_point()+
#   scale_y_continuous(trans='log10')+
#   facet_grid(cols = vars(country))+
#   theme_THF()+
#   scale_fill_THF()+
#   scale_colour_THF()+
#   labs(x= "Deprivation", y="Mortality Hazard Ratio (log-scaled)", title="")+
#   theme(plot.title = element_text(size=14),
#         legend.text=element_text(size=14), legend.position="bottom", 
#         axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
#         strip.text=element_text(size=12), axis.line.x = element_line(colour='grey20'))


df %>% 
  mutate(lab_country=factor(country, levels=c("England", "Canada (Ontario)"), labels=c("England", "Ontario (Canada)")),
         lab_age=factor(age_group, levels=c("Working age (18-64 years)", "Older age (65+ years)"), 
                        labels=c("Working age adults (18-64 years)", "Older adults (65+ years)")),
         lab_dep=factor(deprivation, levels=c(1:10), labels=c("1-Least Deprived", 2:9, "10-Most Deprived"))) %>% 
  ggplot(., aes(x=lab_dep, y=hr, group=lab_age, colour=lab_age))+
  geom_line()+
  geom_point()+
  scale_y_continuous(trans='log10')+
  facet_grid(cols = vars(lab_country))+
  theme_THF()+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "Deprivation", y="Mortality Hazard Ratio (log-scaled)", title="")+
  theme(plot.title = element_text(size=14),
        legend.text=element_text(size=14), legend.position="bottom", 
        axis.text.x=element_text(size=10, angle = 80, hjust=0.5),axis.text.y=element_text(size=14),
        strip.text=element_text(size=12), axis.line.x = element_line(colour='grey20'))


ggsave(str_c(outputs_path, 'deprivation_graph_log_scale.png'),dpi=300,
       width = 10, height =6.5)



# Figure 2 and 3- interaction graphs -------------------------------------------------------

##load data


canada_working_interact<-read_excel(str_c(canada_path, 'CA_results_models.xlsx'),sheet='Model 4a', skip=7)

canada_older_interact<-read_excel(str_c(canada_path, 'CA_results_models.xlsx'),sheet='Model 4b', skip=7)

england_working_interact<-read_excel(str_c(england_path, 'anne_table4a.xls'))

england_older_interact<-read_excel(str_c(england_path, 'anne_table4b.xls'))


##Data cleaning 


##working adults: Canada

CW_raw<-canada_working_interact %>% 
    remove_empty(c("rows", "cols")) %>% 
    clean_names()  %>% 
    mutate(term=ifelse(is.na(x1), t,x1)) %>% 
    select(c(term,estimate=coef,lower_ci=p_z,upper_ci=x95_percent)) %>% 
    mutate(hr=exp(estimate), hr_lower=exp(lower_ci), hr_upper=exp(upper_ci)) %>%
    filter(!(is.na(estimate))) %>% 
    select(term, estimate) %>% 
  pivot_wider(names_from = term, values_from=estimate)

CW_dep<-CW_raw %>% 
  select(D01:D10) %>% 
  pivot_longer(c(D01:D10),names_to="term", values_to="estimate") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

CW_interact<-CW_raw %>% 
  select(`1xdx`:`10xdx`) %>% 
  pivot_longer(c(`1xdx`:`10xdx`),names_to="term", values_to="interact") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

CW_combined<-CW_dep %>% 
  merge(CW_interact, by='dep') %>% 
  mutate(conds=CW_raw$dxcount6)

CW_new<-do.call("rbind", replicate(7,CW_combined, simplify = FALSE))

CW_new<-CW_new %>% 
  mutate(num=(rep(0:6,each=10)),hr=exp(estimate+(num*interact)+(num*conds)))


##older adults: Canada

CO_raw<-canada_older_interact %>% 
  remove_empty(c("rows", "cols")) %>% 
  clean_names()  %>% 
  mutate(term=ifelse(is.na(x1), t,x1)) %>% 
  select(c(term,estimate=coef,lower_ci=p_z,upper_ci=x95_percent)) %>% 
  mutate(hr=exp(estimate), hr_lower=exp(lower_ci), hr_upper=exp(upper_ci)) %>%
  filter(!(is.na(estimate))) %>% 
  select(term, estimate) %>% 
  pivot_wider(names_from = term, values_from=estimate)

CO_dep<-CO_raw %>% 
  select(D01:D10) %>% 
  pivot_longer(c(D01:D10),names_to="term", values_to="estimate") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

CO_interact<-CO_raw %>% 
  select(`1xdx`:`10xdx`) %>% 
  pivot_longer(c(`1xdx`:`10xdx`),names_to="term", values_to="interact") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

CO_combined<-CO_dep %>% 
  merge(CO_interact, by='dep') %>% 
  mutate(conds=CO_raw$dxcount6)

CO_new<-do.call("rbind", replicate(7,CO_combined, simplify = FALSE))

CO_new<-CO_new %>% 
  mutate(num=(rep(0:6,each=10)),hr=exp(estimate+(num*interact)+(num*conds)))

##working adults: England

EW_raw<-england_working_interact %>% 
  remove_empty(c("rows", "cols")) %>% 
  clean_names()  %>% 
  select(c(term,estimate)) %>% 
  filter(!(is.na(estimate))) %>% 
  pivot_wider(names_from = term, values_from=estimate)

EW_dep<-EW_raw %>% 
  select(`imd2015_101`:`imd2015_1010`) %>% 
  pivot_longer(c(`imd2015_101`:`imd2015_1010`),names_to="term", values_to="estimate") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

EW_interact<-EW_raw %>% 
  select(`imd2015_101:total_ca_cap`:`imd2015_1010:total_ca_cap`) %>% 
  pivot_longer(c(`imd2015_101:total_ca_cap`:`imd2015_1010:total_ca_cap`),names_to="term", values_to="interact") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

EW_combined<-EW_dep %>% 
  merge(EW_interact, by='dep') %>% 
  mutate(conds=EW_raw$total_ca_cap)

EW_new<-do.call("rbind", replicate(7,EW_combined, simplify = FALSE))

EW_new<-EW_new %>% 
  mutate(num=(rep(0:6,each=10)),hr=exp(estimate+(num*interact)+(num*conds)))


##older age adults: England

EO_raw<-england_older_interact %>% 
  remove_empty(c("rows", "cols")) %>% 
  clean_names()  %>% 
  select(c(term,estimate)) %>% 
  filter(!(is.na(estimate))) %>% 
  pivot_wider(names_from = term, values_from=estimate)

EO_dep<-EO_raw %>% 
  select(`imd2015_101`:`imd2015_1010`) %>% 
  pivot_longer(c(`imd2015_101`:`imd2015_1010`),names_to="term", values_to="estimate") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

EO_interact<-EO_raw %>% 
  select(`imd2015_101:total_ca_cap`:`imd2015_1010:total_ca_cap`) %>% 
  pivot_longer(c(`imd2015_101:total_ca_cap`:`imd2015_1010:total_ca_cap`),names_to="term", values_to="interact") %>% 
  mutate(dep=c(1:10)) %>% 
  select(-term)

EO_combined<-EO_dep %>% 
  merge(EO_interact, by='dep') %>% 
  mutate(conds=EO_raw$total_ca_cap)

EO_new<-do.call("rbind", replicate(7,EO_combined, simplify = FALSE))

EO_new<-EO_new %>% 
  mutate(num=(rep(0:6,each=10)),hr=exp(estimate+(num*interact)+(num*conds)))


##Merge data sets

CW_new<- CW_new %>% 
  mutate(age_group=rep("Working age (18-64 years)",70), country=rep("Canada (Ontario)",70)) 

# %>% 
#   select(dep,num, hr, age_group, country)

CO_new<- CO_new %>% 
  mutate(age_group=rep("Older age (65+ years)",70), country=rep("Canada (Ontario)",70)) 

# %>% 
#   select(dep, num, hr, age_group, country)

EW_new<- EW_new %>% 
  mutate(age_group=rep("Working age (18-64 years)",70), country=rep("England",70)) 

# %>% 
#   select(dep, num, hr, age_group, country)

EO_new<- EO_new %>% 
  mutate(age_group=rep("Older age (65+ years)",70), country=rep("England",70)) 

# %>% 
#   select(dep, num, hr, age_group, country)

df.interact<-rbind(CW_new, CO_new, EW_new, EO_new)

df.interact$log_hr<-log(df.interact$hr)
df.interact$coef_tot<-(df.interact$conds*df.interact$num)+df.interact$estimate+(df.interact$interact*df.interact$num)


# df.interact %>% 
#   filter(age_group=="Older age (65+ years)") %>% 
#   filter(dep %in% c(1,10) & num %in% c(2,4))

df.interact %>%
  filter(age_group=="Older age (65+ years)") %>%
  mutate(lab=factor(num, levels=c(0:6), labels=c(0:5,"6+")),
         lab_dep= factor(dep, levels=c(1:10), labels=c("1-Least Deprived", c(2:9), "10-Most Deprived")),
         lab_country=factor(country, levels=c("England", "Canada (Ontario)"), labels=c("England", "Ontario (Canada)"))) %>%
  ggplot(., aes(x=lab, y=hr, group=lab_dep))+
  geom_col(aes(colour=lab_dep, fill = lab_dep), position = position_dodge(0.8), width = 0.7)+
  scale_y_continuous(trans='log10')+
  facet_grid(cols= vars(lab_country))+
  theme_THF()+
  scale_color_manual(values=cc)+
  scale_fill_manual(values=cc)+
  labs(x= "Number of conditions", y="Mortality Hazard Ratio (log-scaled)", title="")+
  guides(color = guide_legend(nrow = 1))+
  theme(plot.title = element_text(size=16),
        legend.text=element_text(size=16), legend.position="bottom", legend.box = 'horizontal',
        axis.text.x=element_text(size=16, angle = 0, hjust=0.45),axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text=element_text(size=16), axis.line.x = element_line(colour='grey20'))


# df.interact %>% 
#   filter(age_group=="Older age (65+ years)") %>% 
#   mutate(lab=factor(num, levels=c(0:6), labels=c(0:5,"6+")),
#          lab_dep= factor(dep, levels=c(1:10), labels=c("1-Least Deprived", c(2:9), "10-Most Deprived")),
#          lab_country=factor(country, levels=c("England", "Canada (Ontario)"), labels=c("England", "Canada (Ontario)"))) %>% 
#   ggplot(., aes(x=lab, y=hr, group=lab_dep))+
#   geom_col(aes(colour=lab_dep, fill = lab_dep), position = position_dodge(0.8), width = 0.7)+
#   scale_y_continuous(trans='log10')+
#   facet_grid(cols= vars(lab_country))+
#   theme_THF()+
#   scale_color_manual(values=cc)+
#   scale_fill_manual(values=cc)+
#   labs(x= "Number of conditions", y="Mortality Hazard Ratio (log-scaled)", title="Older adults (65+ years)")+
#   guides(color = guide_legend(nrow = 1))+
#   theme(plot.title = element_text(size=8), axis.title = element_text(size=6),
#         legend.text=element_text(size=5), legend.position="bottom", legend.box = 'horizontal',
#         axis.text.x=element_text(size=5, angle = 0, hjust=0.45),axis.text.y=element_text(size=5),
#         strip.text=element_text(size=5), axis.line.x = element_line(colour='grey20'),
#         legend.key.size= unit(0.2,"cm"))


ggsave(str_c(outputs_path, 'interaction_older_graph_log.png'),dpi=300,
       width = 16, height =11.5)

  # theme(plot.title = element_text(size=14),
  #       legend.text=element_text(size=8), legend.position="bottom", legend.box = 'horizontal',
  #       axis.text.x=element_text(size=12, angle = 0, hjust=0.45),axis.text.y=element_text(size=12),
  #       strip.text=element_text(size=12), axis.line.x = element_line(colour='grey20'))


# 
# ggsave(str_c(outputs_path, 'interaction_older_graph_log_scale.png'),dpi=300,
#        width = 10, height =6.5)


df.interact %>% 
  filter(age_group=="Working age (18-64 years)") %>% 
  mutate(lab=factor(num, levels=c(0:6), labels=c(0:5,"6+")),
         lab_dep= factor(dep, levels=c(1:10), labels=c("1-Least Deprived", c(2:9), "10-Most Deprived")),
         lab_country=factor(country, levels=c("England", "Canada (Ontario)"), labels=c("England", "Ontario (Canada)"))) %>% 
  ggplot(., aes(x=lab, y=hr, group=lab_dep))+
  geom_col(aes(colour=lab_dep, fill = lab_dep), position = position_dodge(0.8), width = 0.7)+
  scale_y_continuous(trans='log10')+
  facet_grid(cols= vars(lab_country))+
  theme_THF()+
  scale_color_manual(values=cc)+
  scale_fill_manual(values=cc)+
  labs(x= "Number of conditions", y="Mortality Hazard Ratio (log-scaled)", title="")+
  guides(color = guide_legend(nrow = 1))+
  theme(plot.title = element_text(size=16),
        legend.text=element_text(size=16), legend.position="bottom", legend.box = 'horizontal',
        axis.text.x=element_text(size=16, angle = 0, hjust=0.45),axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text=element_text(size=16), axis.line.x = element_line(colour='grey20'))


ggsave(str_c(outputs_path, 'interaction_working_graph_log2.png'),dpi=300,
       width = 16, height =11.5)


# Supplementary figure 2- number of conditions linear graph ---------------------------------------

##load data 

canada<-read_excel(str_c(canada_path, 'CA_results_sensitivity_analysis.xlsx'),sheet='ltcs', skip=7)

england<-read_excel(str_c(england_path, 'anne_suppltable3.xls'))


#data cleaning 
canada_raw<-canada %>% 
  clean_names() %>% 
  select(c(term=t,estimate=coef,lower_ci=p_z,upper_ci=x95_percent)) %>% 
  mutate(hr=exp(estimate), hr_lower=exp(lower_ci), hr_upper=exp(upper_ci)) %>%
  filter(!(is.na(estimate))) %>% 
  filter(term %in% c("REF", 1:5, "6+")) %>% 
  mutate(country="Canada (Ontario)", num=c(0:6)) %>% 
  select(hr, country, num)

england_raw<-england %>% 
  remove_empty(c("rows", "cols")) %>% 
  clean_names()  %>% 
  filter_all(any_vars(str_detect(., 'total_ca_cap'))) %>% 
  select(term, estimate)

england_zero<-c('total_ca_cap_cat0',0)
england_raw<-rbind(england_raw, england_zero)

england_raw<-england_raw %>% 
  mutate(country="England", num=c(1:6,0), hr=exp(as.numeric(estimate))) %>% 
  select(hr, country, num) %>% 
  arrange(., num)

df<-rbind(canada_raw, england_raw)

#data visualisation


df %>% 
  mutate(lab_country=factor(country, levels=c("England", "Canada (Ontario)"), labels=c("England", "Ontario (Canada)")),
         lab_num=factor(num, levels=c(0:6), labels=c(0:5, "6+"))) %>% 
  ggplot(., aes(x=lab_num, y=hr, group=country, colour=country))+
  geom_line()+
  geom_point()+
  facet_grid(rows = vars(lab_country))+
  scale_y_continuous(trans='log10')+
  theme_THF()+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "Number of conditions", y="Mortality Hazard Ratio (log-scaled)", title="")+
  theme(plot.title = element_text(size=16),
        legend.text=element_text(size=16), legend.position="none", 
        axis.text.x=element_text(size=16, angle = 0, hjust=0.45),axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text=element_text(size=16), axis.line.x = element_line(colour='grey20'))


ggsave(str_c(outputs_path, 'number_conds_linear_graph.png'),dpi=300,
       width = 16, height =11.5)



# supplementary figure 1- Distirbution of number of conditions ------------------------------------------------

canada_all<-read.delim(str_c(canada_path, 'number_of_conditions_capped_canada.txt'), nrows=10)
canada_working<-read.delim(str_c(canada_path, 'number_of_conditions_capped_canada.txt'), skip=18, nrows=10)
canada_older<-read.delim(str_c(canada_path, 'number_of_conditions_capped_canada.txt'), skip=33, nrows=10)

txt.clean(df=canada_all)
txt.clean(df=canada_working)
txt.clean(df=canada_older)

canada_older$age<-"Older adults (65+ years)"
canada_working$age<-"Working age (18-64 years)"


england_working<-read_csv(str_c(england_path, 'anne_suppltable11a.csv'))
england_older<-read_csv(str_c(england_path, 'anne_suppltable11b.csv'))


england_working<-england_working %>% 
  select(c(cat,n,percent))

names(england_working)[1:3]<-c("cons", "freq", "percent")
england_working$age<-"Working age (18-64 years)"

england_older<-england_older %>% 
  select(c(cat,n,percent))

names(england_older)[1:3]<-c("cons", "freq", "percent")

england_older$age<-"Older adults (65+ years)"

canada<-rbind(canada_working, canada_older)

canada$country<-"Ontario (Canada)"

england<-rbind(england_working, england_older)

england$country<-"England"

df<-rbind(canada, england)


df<-df %>% 
  group_by(age,country) %>% 
  mutate(n_total=sum(freq)) %>% 
  mutate(prop=freq/n_total)

df %>% 
  mutate(age_lab=factor(age, levels=c("Working age (18-64 years)", "Older adults (65+ years)"))) %>% 
  ggplot(aes(x=cons, y=prop, colour=country, fill=country))+
  geom_col()+
  facet_grid(rows=vars(country), cols=vars(age_lab))+
  scale_y_continuous(labels=scales::percent)+
  theme_THF()+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "Number of conditions", y="Distribution of number of conditions (%)", title="")+
  theme(plot.title = element_text(size=14),
        legend.text=element_text(size=14), legend.position="none", 
        axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
        strip.text=element_text(size=12), axis.line.x = element_line(colour='grey20'))

ggsave(str_c(outputs_path, 'prop_graphs.png'),dpi=300,
       width = 10, height =6.5)

