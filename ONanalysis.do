
 
Socioeconomic gradient in mortality of working age and older adults with 
multiple long-term conditions in England and Ontario, Canada

Stata code for the Ontario analysis. Modelling only. Descriptive statistics 
were done in SAS using in-house macros. 

/*******************************************************************************
© 2022 ICES. All rights reserved.

TERMS OF USE:
 
##Not for distribution.## This code and data is provided to the user solely for 
its own non-commercial use by individuals and/or not-for-profit corporations. 
User shall not distribute without express written permission from ICES.

##Not-for-profit.## This code and data may not be used in connection with profit 
generating activities.

##No liability.## ICES makes no warranty or representation regarding the 
 fitness, quality or reliability of this code and data. 

##No Support.## ICES will not provide any technological, educational or 
 informational support in connection with the use of this code and data. 

##Warning.## By receiving this code and data, user accepts these terms, and uses 
the code and data, solely at its own risk. 
*******************************************************************************/

// setup
clear all
set more off, perm
set cformat %6.3f
set pformat %4.3f
set sformat %6.3f

use "/path/ontario_cohort.dta", clear
rename dx_green mltc
drop if age==.
drop if women==.
drop if deprivation_d_da==.
drop if mltc=.
compress

gen dxcount6=mltc
replace dxcount6=6 if mltc>6

capture stset, clear 
stset tte, failure(died==1) scale(365.25) id(ikn)

// check age linearity
capture drop mg
stcox age, nohr efron mgale(mg)
lowess mg age

// check mltc linearity
capture drop mg
stcox mltc, nohr efron mgale(mg)
lowess mg mltc
stcox i.women c.age i.mltc, base efron  
coefplot, drop(0.women 1.women age) vertical base noci eform recast(connected) grid(within)

capture drop mg
stcox dxcount6, nohr efron mgale(mg)
lowess mg dxcount6
stcox i.women c.age i.dxcount6, base efron  
coefplot, drop(0.women 1.women age) vertical base noci eform recast(connected) grid(within)

// tests for proportional hazards
stcox i.women , base nolog efron 
 estat phtest, detail
stcox c.age, base nolog efron 
 estat phtest, detail 
stcox c.dxcount6 , base nolog efron 
 estat phtest, detail 
stcox i.deprivation_d_da , base nolog efron 
 estat phtest, detail 
glo varlist i.women c.age c.dxcount6 i.deprivation_d_da 
stcox $varlist , base nolog efron  
 estat phtest, detail 

// check schoenfeld residuals of last model 
estat phtest, plot(1.women) 
estat phtest, plot(age) 
estat phtest, plot(dxcount6) 
estat phtest, plot(2.deprivation_d_da) 
estat phtest, plot(3.deprivation_d_da) 
estat phtest, plot(4.deprivation_d_da) 
estat phtest, plot(5.deprivation_d_da) 
estat phtest, plot(6.deprivation_d_da) 
estat phtest, plot(7.deprivation_d_da) 
estat phtest, plot(8.deprivation_d_da) 
estat phtest, plot(9.deprivation_d_da)
estat phtest, plot(10.deprivation_d_da) 

// loglog plot of survival
stphplot, by(women) 
stphplot, by(agegrp)
stphplot, by(dxcount6) 
stphplot, by(deprivation_d_da)
 
// test time dependent coefficients for age and mltc 
// by ln(time) 
stcox $varlist , base efron nolog tvc(c.age c.dxcount6) texp(ln(_t))
 lrtest, saving(0) 
quietly stcox $varlist , base efron nolog 
 lrtest, using(0) 

// at defined times (1 year, 2.5 years, 4 years) 
foreach i of numlist 1 2.5 4 {
stsplit splittime, at(0,`i')
gen age_t=age*(splittime==`i')
gen dx_t=dxcount6*(splittime==`i')
stcox $varlist c.age_t c.dx_t , base efron nolog
 estat phtest, detail
 capture drop age_t dx_t
qui gen age_t0=age*(splittime==0)
qui gen age_t1=age*(splittime==`i')
qui gen dx_t0=dxcount6*(splittime==0)
qui gen dx_t1=dxcount6*(splittime==`i')
stcox i.women i.deprivation_d_da c.age_t0 c.age_t1 c.dx_t0 c.dx_t1, base efron nolog
 estat phtest, detail 
 capture drop age_t* dx_t*
stcox $varlist , base efron nolog tvc(c.age c.dxcount6) texp(_t>`i')
capture drop splittime
stjoin
}
 
// primary analyses 
stcox c.age i.deprivation_d_da i.women, base efron nolog 
 estimates store m1
stcox c.age##i.deprivation_d_da i.women, base efron nolog
 estimates store m2
 lrtest m1
 
// stratified by older/younger adult 
forval i=0/1 {
preserve
 keep if olderadult==`i'
 stcox i.deprivation_d_da i.women c.age, base efron nolog 
  estimates store m3
 stcox i.deprivation_d_da i.women c.dxcount6 c.age, base efron nolog
  estimates store m4
  lrtest m3
 stcox i.deprivation_d_da##c.dxcount6 i.women c.age, base efron nolog
  estimates store m5
  lrtest m4
restore 
}

// stratified by sex 
forval i=0/1 {
preserve
 keep if women==`i'
 stcox i.deprivation_d_da c.age, base efron nolog 
  estimates store m3
 stcox i.deprivation_d_da c.dxcount6 c.age, base efron nolog
  estimates store m4
  lrtest m3
 stcox i.deprivation_d_da##c.dxcount6 c.age, base efron nolog
  estimates store m5
  lrtest m4
restore 
}

// figure 1, run as stratified models 
// to verify against interaction, turn on mlabel option
// done in R using model results 
// preserve 
// lab define depdec 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 
// lab values deprivation_d_da depdec 
// qui stcox i.women c.age i.deprivation_d_da if olderadult==0, base efron
//  est store A
// qui stcox i.women c.age i.deprivation_d_da if olderadult==1, base efron
//  est store B
// glo graphlook scheme(s2color) graphregion(color(white))
// glo copts recast(connected) drop(0.women 1.women age) base noci offset(0)
// coefplot (A, label("Working age adults (18-64y)") $copts pstyle(p2)) ///
//  (B, label("Older age adults (65y+)") $copts pstyle(p1)) ,  $graphlook ///
//  xti("Decile of material deprivation") yti("Mortality hazard ratio") ///
//  vertical eform ylab(1(0.2)2.4) /* mlabel */
// restore 







