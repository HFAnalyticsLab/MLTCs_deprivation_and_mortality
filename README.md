# Socioeconomic gradient in mortality of working age and older adults with multiple long-term conditions in England and Ontario, Canada

#### Project Status: [In progress]

## Project Description

The number of people living with multiple long-term conditions is rising in many western countries and is socially patterned; those living in deprived areas have higher prevalence with a faster and earlier acquisition rate. There is currently mixed evidence on the influence of number of conditions and deprivation on mortality. We aimed to explore whether number of long-term conditions contribute to socioeconomic inequalities in mortality, examine whether the influence of number of conditions on mortality is consistent across socioeconomic groups and assess whether these associations vary by working age (18-64 years) and older adults (65+ years). We provide a cross-jurisdiction comparison between England and Ontario, by replicating the analysis using comparable representative data from both jurisdictions. 

Survival time based on deaths from all causes is the primary outcome for this study. Cox regression models were used to estimate hazards of mortality by number of long-term conditions, deprivation and their interaction, with adjustment for age and sex and stratified between working age and older adults in England

## Data source

We used data from the Clinical Practice Research Datalink (CPRD Aurum) linked to Hospital Episode Statistics, IMD mortality and Indices of Multiple Depriavtion (eRAP 20_000239).

The codelists we used to derive conditions for patients are from [Anna Head](https://github.com/annalhead) at the University of Liverpool and can be found [here](https://github.com/annalhead/CPRD_multimorbidity_codelists).

This study used health administrative data from Ontario (information to be added)

## How does it work?

### England analysis (completed in R) 

#### Requirements

These scripts were written in R version 4.0.5 and RStudio Version 1.1.383.

The following R packages (available on CRAN) are needed: 
* [**tidyverse**](https://www.tidyverse.org/)
* [**data.table**](https://cran.r-project.org/web/packages/data.table)
* [**survival**](https://cran.r-project.org/web/packages/survival/index.html)
* [**survminer**](https://cran.r-project.org/web/packages/survminer/index.html)

In addition these scripts make use of our in house package [**aurumpipeline**](https://github.com/HFAnalyticsLab/aurumpipeline) available here on GitHub.



### Ontario analysis (completed in Stata) 

(to be added) 



### Getting started

If you have access to a CPRD Aurum sample then you can run the scripts in order to re-produce this work. For ontario, the computer programs may rely upon coding templates or macros that are unique to ICES and are therefore either inaccessible or may require modification.

## Authors

Add link to paper [here]

* Anne Alarilla - [Twitter](https://twitter.com/alarillaanne)
* Luke Mondor 
* Jay Hughes - [GitHub](https://github.com/Jay-ops256)
* Mai Stafford, PhD - [Twitter](https://twitter.com/stafford_xm)



## License

This project is licensed under the MIT License
