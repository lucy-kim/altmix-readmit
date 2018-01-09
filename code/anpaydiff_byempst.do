*what is the pay differential between regular & (directly hired) contingent nurses?

loc reg /home/hcmg/kunhee/Labor/regresults
cd /home/hcmg/kunhee/Labor/Bayada_data


use weekly_workerpanel, clear

drop if status==""
assert status!=""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab values st stl

drop if year(monday) < 2012
/* keep if monday >= mdy(1,2,2012) & monday <= mdy(9,21,2015) */

/* keep if attrited==1 */

gen pervisit_rate = salary/productivity

/* *aggregate to the weekly level because pay data are in weekly level
collapse (mean) payrate salary pervisit_rate productivity (sum) visit_points (max) tenure enter exit worked, by(payrollno monday esd st offid_nu lemp fired female race age admiss_coordi eval_producti st attrited) */

tempfile worker
save `worker'

*in the weekly office-payrollno-discipline data, keep only SN obs
use pay_bywwod, clear
keep if paydisc=="RN" | paydisc=="LPN"
drop paydisc
collapse (sum) pay_*, by(offid_nu payrollno startd endd njobs noffices)
rename startd monday
rename endd sunday
tempfile pay_bywwo
save `pay_bywwo'

*merge the weekly worker panel on tenure, number of visits, status with the weekly pay data
use `worker', clear
merge 1:1 offid_nu payrollno monday using `pay_bywwo', keep(1 3)
tab _merge
*56K _m=3; 7K _m=1

*create weekly & per-visit rate values for non-salaried workers
gen nonsalary = pay_REGULAR + pay_BONUS + pay_OVERTIME
gen nonsalary_pv = nonsalary/wnvp

*create all visit points provided for each worker-week across offices
bys payrollno monday: egen allvp = sum(wnvp)

*create Percentage of visit productivity goals met
gen prod_met = 100*allvp / productivity
lab var prod_met "% visit productivity goals met"

gen wkpay = salary if st==1 | st==2 | st==3
replace wkpay = nonsalary if st==4 | st==5 | st==6

gen wage = nonsalary_pv if  st==4 | st==5 | st==6
replace wage = pervisit_rate if st==1 | st==2 | st==3

lab var tenure "Tenure in the firm (months)"
lab var wage "Per visit rate ($)"
lab var wkpay "Total weekly pay ($)"
lab var prod_met "% weekly productivity met"

gen white = race=="WHITE"

/* foreach v of varlist wkpay wage {
    gen ln`v' = log(`v')
}

forval x=2/4 {
    gen tenure`x' = tenure^`x'
} */

lab var white "White"
/* lab var tenure2 "Tenure squared"
lab var tenure3 "Tenure, 3rd power"
lab var tenure4 "Tenure, 4th power" */

compress
save hr, replace

*---------- Analysis
/* use hr, clear

*aggregate to salaried vs piece-rate
gen salaried = st==1 | st==2 | st==3

*merge with panel demand FL data to restrict to the same sample period & offices
drop _merge
merge m:1 offid_nu monday salaried using panel_demandfl, keep(2 3)
keep if type==1 | type==3

bys established: sum wkpay wage tenure lemp productivity visit_points if salaried==1
bys established: sum wkpay wage tenure lemp productivity visit_points if salaried==0

lab var tenure "Tenure in the firm (months)"
lab var wage "Per visit rate ($)"
lab var wkpay "Total weekly pay ($)"
lab var prod_met "% weekly productivity met"
lab var productivity "Visit productivity goal"
lab var lemp "Employment length (mo.)"
lab var visit_points "Visit points"

forval x = 0/1 {
    preserve
    keep if salaried==`x'
    keep established wkpay wage tenure lemp productivity visit_points
    order wkpay wage tenure lemp productivity visit_points
    bys established: outreg2 using `reg'/summ_byestab2_sa`x'.xls, replace sum(log) eqkeep(N mean sd) tex label
    restore
}

tempfile t
save `t'

forval x = 0/1 {
    use `t', clear

    tempname ttestparm
    tempfile outfile

    postfile `ttestparm' Variable n1 n2 mu1 mu2 sd1 sd2 diff_b diff_se diff_p using `outfile',replace

    loc i = 0
    foreach v of varlist wkpay wage tenure lemp productivity visit_points {
        loc i = `i' + 1
        ttest `v' if salaried==`x', by(established) unequal
        post `ttestparm' (`v') (`r(N_1)') (`r(N_2)') (`r(mu_1)') (`r(mu_2)') (`r(sd_1)') (`r(sd_2)') (`r(mu_1)'-`r(mu_2)') (`r(se)') (`r(p)')
    }

    postclose `ttestparm'
    preserve
    use `outfile', clear

    export excel using "`reg'/ttestout2_sa`x'.xls", firstrow(variables) replace
    restore
}





loc file paydiff
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

lab var tenure "Tenure (months)"
lab var tenure2 "Tenure squared"
lab var tenure3 "Tenure, 3rd power"
lab var tenure4 "Tenure, 4th power"

areg lnwage established tenure* i.female i.white i.stN if salaried==0, vce(cluster offid_nu) absorb(monday)
areg lnwage established tenure* i.female i.white i.stN if salaried==1, vce(cluster offid_nu) absorb(monday)


*regress pay on employment arrangement dummies, office FE, week FE
*use for pay = pervisit rate or weekly pay with controlling for the weekly number of visits
loc xvar1
loc xvar2 tenure*
loc xvar3 tenure* i.female i.white

forval x=3/3 {
    areg lnwage i.st `xvar`x'' i.monday, absorb(offid_nu) vce(cluster offid_nu)

    loc addtxt "Office fixed effects, Y, Worker fixed effects, ., Week fixed effects, Y"
    `out' keep(2.st 3.st 4.st `xvar`x'') addtext(`addtxt')
}
forval x=1/3 {
    areg lnwage i.st `xvar`x'' i.monday if age >= 25 & age <= 55, absorb(offid_nu) vce(cluster offid_nu)
    loc addtxt "Office fixed effects, Y, Worker fixed effects, ., Week fixed effects, Y"
    `out' keep(2.st 3.st 4.st `xvar`x'') addtext(`addtxt')
}
/* forval x=1/3 {
    areg lnwage i.st `xvar`x'' i.monday i.offid_nu if age >= 25 & age <= 55, absorb(offid_nu) vce(cluster payrollno)
    loc addtxt "Office fixed effects, Y, Worker fixed effects, Y, Week fixed effects, Y"
    `out' keep(2.st 3.st 4.st `xvar`x'') addtext(`addtxt')
} */

*use pay = weekly pay with control for weekly # visits
loc file paydiff2
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

gen vp = visit_points if st==2 | st==4 | st==3
replace vp = productivity if st==1

loc xvar1 vp
loc xvar2 vp tenure*
loc xvar3 vp tenure* i.female i.white

forval x=1/3 {
    areg lnwkpay i.st `xvar`x'' i.monday, absorb(offid_nu) vce(cluster offid_nu)

    loc addtxt "Office fixed effects, Y, Worker fixed effects, ., Week fixed effects, Y"
    `out' keep(2.st 3.st 4.st `xvar`x'') addtext(`addtxt')
}
forval x=1/3 {
    areg lnwkpay i.st `xvar`x'' i.monday if age >= 25 & age <= 55, absorb(offid_nu) vce(cluster offid_nu)
    loc addtxt "Office fixed effects, Y, Worker fixed effects, ., Week fixed effects, Y"
    `out' keep(2.st 3.st 4.st `xvar`x'') addtext(`addtxt')
} */
/* forval x=1/3 {
    areg lnwkpay i.st `xvar`x'' i.monday i.offid_nu if age >= 25 & age <= 55, absorb(offid_nu) vce(cluster payrollno)
    loc addtxt "Office fixed effects, Y, Worker fixed effects, Y, Week fixed effects, Y"
    `out' keep(2.st 3.st 4.st `xvar`x'') addtext(`addtxt')
} */
