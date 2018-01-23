*analyze whether and how contingent and regular nurses differ in the home health care settings in various dimensions
* what is the difference between permanent and temporary nurses? Compare visit loads, salary, tenure

loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'
loc mvar admissionclie
loc nepi_byo_thresh 30
loc nv_SN_thresh 20
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
set matsize 11000

*if there is selection problem, we do not know if is ability or (incentives embedded in) the employment arrangement that makes such difference. By looking at the same worker who changes statuses, we may be able to control for the initial selection of workers with different abilities into different arrangements. So do the analysis using all workers and workers with multiple statuses separately.

*ignoring selection: treat worker-arrangement as a different arrangement
*this treatment could be problematic when counting tenure for workers who have multiple arrangements -> I can still just add the tenure in the first arrangement to the tenure for the next arrangement

*the unit of analysis: worker-office-day or worker-office-week

*----------------------------------------
* difference in work characteristics
*----------------------------------------

/* *get a list of offices in the analysis of the effect of labor mix on outcome
use anivlmis_hosp, clear
keep offid_nu
duplicates drop
count
tempfile office
save `office' */

* per day analysis------------------
* conditional on working: per day, # visits; travel distance and time; total visit time; mean visit time; total visit travel costs; rate of working in other office
use daily_workerpanel, clear

*restrict to nurses
assert majordisc!=""
keep if majordisc=="SN"

/* *restrict to 67 offices
merge m:1 offid_nu using `office', keep(3) nogen */

*aggregate to salaried, contingent-employee, contingent-contractors, and other categories
assert status!=""
tab payrollno if status==""
drop if status==""

*define contingent workers vs permanent ones
assert status!=""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

*restrict to episodes that started on or before 8/9/2015 - 59 days, i.e. 8/9/2015 is the 60th day of the episode
gen date = mdy(8,9,2015) - 59
count if visitdate <= date
keep if visitdate <= date
*keep if monday >= mdy(2,27,2012) & monday <= mdy(9,28,2015)
/* keep if monday >= mdy(2,6,2012) & monday <= mdy(8,31,2015) */

keep payrollno visitdate_e monday offid_nu st visit_points worked_oo worked dnv
*distance_driven time_driven visit_travel_cost

tempfile tmp
save `tmp'

*worker-day level (regardless of office)
use `tmp', clear
collapse (sum) visit_points dnv (max) worked, by(payrollno visitdate_e st)
tempfile daily
save `daily'

*add in daily total visit time & mean visit time from the visit-level data
use visit_worker_chars, clear

*restrict to episodes that started on or before 8/9/2015 - 59 days, i.e. 8/9/2015 is the 60th day of the episode
gen date = mdy(8,9,2015) - 59
count if visitdate <= date
keep if visitdate <= date

/* *restrict to 67 offices
merge m:1 offid_nu using `office', keep(3) nogen */

assert visitdate!=.

sort epiid offid_nu

*define contingent workers vs permanent ones
drop if status==""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

tempfile t
save `t'

use `t', clear
collapse (sum) dlov = lov (mean) mdlov = lov, by(payrollno visitdate)

*merge back with daily worker panel data
merge 1:m payrollno visitdate using `daily', keep(2 3)

*label vars
lab var dnv "Number of visits"
lab var visit_points "Number of visit points"
lab var dlov "Time spent on visits (hrs)"
lab var mdlov "Mean visit length (hrs)"
lab var distance_driven "Total distance driven (in miles)"
lab var time_driven "Total time driven (in hours)"
lab var visit_travel_cost "Total visit travel costs ($)"
lab var worked "Working in the current office"
lab var worked_oo "Working in a different office"

*conditional on working, per day, # visits; travel distance and time; total visit time; mean visit time; total visit travel costs
preserve
keep if worked==1

bys st: sum dnv visit_points dlov mdlov
*distance_driven time_driven visit_travel_cost

keep dnv visit_points dlov mdlov st
*distance_driven time_driven visit_travel_cost
order st dnv dnv visit_points dlov mdlov
*distance_driven time_driven visit_travel_cost
bysort st: outreg2 using `reg'/diff_workchars.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label
restore

*per day, rate of working and rate of working in other office
preserve
bys st: sum worked worked_oo

keep st worked worked_oo
order st worked worked_oo
bysort st: outreg2 using `reg'/diff_workchars2.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label
restore

* per week analysis------------------
* per week, # visits; # visits for discharge or initiation; # days worked; travel distances and time; total visit time; # unique patients; mean visit time; total visit travel costs
use `tmp', clear
collapse (sum) dnv visit_points (max) worked, by(st payrollno monday)

tempfile tmp2
save `tmp2'

*get weekly data on total time spent on visits & mean visit length for each worker-office-week
use `t', clear

*keep if monday >= mdy(2,27,2012) & monday <= mdy(9,28,2015)
/* keep if monday >= mdy(2,6,2012) & monday <= mdy(8,31,2015) */

*how many days per week worked
sort payrollno offid_nu visitdate
bys payrollno offid_nu visitdate: gen workyes = _n==1

*how many days per week worked, sum and mean length of visits
collapse (sum) dlov = lov dpwworked = workyes (mean) mdlov = lov, by(payrollno monday)

*merge back with weekly worker panel data
merge m:1 payrollno monday using `tmp2', keep(2 3)

*replace # work days per week to zero if not working in that week
assert worked!=.
replace dpwworked = 0 if worked==0 & _merge==2

*label vars
lab var dnv "Number of visits"
lab var visit_points "Number of visit points"
lab var dlov "Time spent on visits (hrs)"
lab var mdlov "Mean visit length (hrs)"
lab var distance_driven "Total distance driven (in miles)"
lab var time_driven "Total time driven (in hours)"
lab var visit_travel_cost "Total visit travel costs ($)"
lab var dpwworked "Number of days worked"
lab var worked "Working in the current office"
lab var worked_oo "Working in a different office"

*don't condition on working; count not working week as providing 0 visits & count how many visits / week does a nurse provide each week on average?
bys st: sum dpwworked
tab st, summarize(dpwworked)
tab st, summarize(dnv)

*conditional on working, per week, # visits; travel distance and time; total visit time; mean visit time; total visit travel costs
preserve
keep if worked==1

bys st: sum dnv visit_points dpwworked dlov mdlov
*distance_driven time_driven visit_travel_cost

keep dnv visit_points dpwworked dlov mdlov st
*distance_driven time_driven visit_travel_cost
order st dnv dnv visit_points dpwworked dlov mdlov
*distance_driven time_driven visit_travel_cost
bysort st: outreg2 using `reg'/diff_workchars3.xls, replace sum(log) eqkeep(N mean) dec(2) tex label
restore

*per day, rate of working and rate of working in other office
preserve
bys st: sum worked worked_oo

keep st worked worked_oo
order st worked worked_oo
bysort st: outreg2 using `reg'/diff_workchars4.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label
restore

*---------------------
*usual serving distance of each office-worker-status

*since the above data is monthly level, aggregate up to office-worker-status
use wmsz_distance, clear
keep offid_nu payrollno st patzip workerzip zip1 zip2 mi_to_zcta5

merge m:1 offid_nu using office67, keep(3) nogen

duplicates drop
tempfile dist
save `dist'

*get # episodes for each worker-patient's ZIP code pair during all time
use visit_worker_chars, clear
merge m:1 offid_nu using office67, keep(3) nogen
merge m:1 payrollno workerID using staff_chars, keepusing(zipcode) keep(1 3) nogen
rename zipcode workerzip

*merge with episode-level data to get patient's ZIP code & unique client ID
merge m:1 epiid using client_chars, keepusing(patzip clientid)
*1M have _m=1; 4M have _m=3
drop if patzip==.

*since I need patient ZIP codes, keep only episodes for which I have ZIP codes
keep if _merge==3
drop _merge

*compute HHI of coverage areas for each worker-status
*define contingent workers vs permanent ones
drop if status==""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

*drop if office ID is missing
drop if offid_nu==.

tempfile tmp3
save `tmp3'

*restrict to established offices
use `tmp3', clear
/* merge m:1 offid_nu using office_restrict, nogen keep(1 3)
keep if established==1 */

*tag episodes: count episode once
bys offid_nu payrollno st patzip epiid : gen j = _n==1
/* sort offid_nu ym_visitd patzip epiid
bys offid_nu ym_visitd patzip epiid: gen j = _n==1 */

collapse (sum) nepi = j, by(offid_nu payrollno workerzip st patzip)

bys offid_nu payrollno st: egen tepi = sum(nepi)

*convert to string var for patient & worker ZIP codes with leading 0's
foreach v of varlist patzip workerzip {
    capture destring `v', replace ig("-")
    gen str5 `v'2 = string(`v', "%05.0f")
    drop `v'
    rename `v'2 `v'
    drop if `v'=="." | `v'==""
}

tempfile nepi
save `nepi'

*merge with office-worker-status-patient ZIP code level
use `nepi', clear
merge 1:1 offid_nu payrollno st patzip workerzip using `dist', nogen
*all matched

gen shepi = nepi/tepi
gen wgtdist = mi_to*shepi

collapse (sum) servdist = wgtdist, by(offid_nu payrollno st)

preserve
keep servdist st
lab var servdist "Nurse's usual serving distance in each month"
bys st: sum servdist
bysort st: outreg2 using `reg'/diff_workchars5.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label
restore

preserve
forval x=1/3 {
    sum servdist if st==`x', de
    drop if st==`x' & servdist > `r(p99)'
}
kdensity servdist if st==1, plot( kdensity servdist if st==2 || kdensity servdist if st==3 & servdist <= 100) legend(cols(3) label(1 "Regular") label(2 "Direct-hire contingent") label(3 "Contractor"))
graph save `gph'/servdist, asis replace
restore

*----------------------------------------
* difference in HR characteristics: pay per visit, tenure, turnover rate
*----------------------------------------
*1) pay per visit: weekly wage per worker (from anwage.do)
use daily_workerpanel, clear

loc labVFT "Full time salaried"
loc labVPB "Part time salaried with benefit"
loc labVPC "Part time salaried without benefit"
loc labVPD "Piece-rate paid"

*restrict to nurses
keep if discipline=="SN"

*restrict to episodes that started on or before 8/9/2015 - 59 days, i.e. 8/9/2015 is the 60th day of the episode
gen date = mdy(8,9,2015) - 59
count if visitdate <= date
keep if visitdate <= date

merge m:1 offid_nu using `office', keep(3) nogen

*define contingent workers vs permanent ones
drop if status==""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

/* drop if year(monday) < 2012 */
/* keep if monday >= mdy(2,6,2012) & monday <= mdy(8,31,2015) */

gen pervisit_rate = salary/productivity

*tag week when the worker attrited
gen exit = etd == visitdate

*tag week when the worker newly hired
gen enter = esd==visitdate

keep if attrited==1

*length of employment for each worker
preserve
replace etd2 = etd if etd2==.
assert etd2!=.
replace esd2 = esd if esd2==.
assert esd2!=.

gen lemp2 = (etd2 - esd2 + 1)/30
keep payrollno lemp* st offid_nu
duplicates drop

*make length of employment to # months (lemp is the entire period of tenure that doesn't vary over time for each worker)
foreach v of varlist lemp {
  replace `v' = `v'*7/30
}

bys st: sum lemp*


*aggregate to the weekly level because pay data are in weekly level
/* collapse (mean) payrate salary pervisit_rate productivity (sum) visit_points distance_driven (max) enter exit worked, by(payrollno monday esd status offid_nu lemp fired female age admiss_coordi eval_producti st) */
collapse (mean) payrate salary pervisit_rate productivity (sum) visit_points (max) enter exit worked, by(payrollno monday etd esd status offid_nu lemp fired female age admiss_coordi eval_producti st)

*on each week , what is the tenure up to that point?
gen tenure = monday - esd + 1
replace tenure = tenure / 30
sum tenure
*there are tenure < 0 ; drop them for now
*drop if tenure < 0

*restrict to weeks the worker was active
keep if worked==1

*make length of employment to # months (lemp is the entire period of tenure that doesn't vary over time for each worker)
replace lemp = lemp*7/30

foreach v of varlist admiss_coordi eval_producti {
    gen `v'2 = `v'==1
    drop `v'
    rename `v'2 `v'
}

tempfile worker
save `worker'

*merge with weekly payrollno-office level pay data
use `worker', clear
rename monday startdate
merge 1:m offid_nu payrollno startdate using pay_bywwod, keep(1 3)

*aggregate to worker-status level
collapse (sum) pay_* (mean) esd fired lemp payrate salary pervisit_rate productivity visit_points enter exit tenure female age admiss_coordi eval_producti, by(payrollno startdate status st)

/* collapse (sum) pay_* (mean) esd fired lemp payrate salary pervisit_rate productivity visit_points distance_driven enter exit tenure female age admiss_coordi eval_producti, by(offid_nu payrollno startdate status st) */
rename startdate monday

gen nonsalary = pay_REGULAR + pay_BONUS + pay_OVERTIME
gen nonsalary_pv = nonsalary/visit_points

/* tempfile hr */
compress
save hr, replace


use hr, clear

*create all visit points provided for each worker-week across offices
bys payrollno monday: egen allvp = sum(visit_points)

*create Percentage of visit productivity goals met
gen prod_met = 100*allvp / productivity
lab var prod_met "% visit productivity goals met"

gen pay = salary if st<=3
replace pay = nonsalary if st >3

*there is no pay information for contractors
bys st: sum salary nonsalary

gen wage = nonsalary_pv if st >3
replace wage = pervisit_rate if st<=3
*there are still missing values in salary or nonsalary for non-contractors
/* tab st if wage==. */

/* gen misswage = wage==.
tab misswage if st==1
tab misswage if st==2
tab misswage if st==4
*contingent employees who have missing nonsalary have salary reported
sum pervisit_rate nonsalary_pv if wage==. & st==2
sum pervisit_rate nonsalary_pv if wage!=. & st==2 */

lab var wage "Per visit rate (\$))"
lab var tenure "Tenure in the firm (months)"
lab var wage "Per visit rate (\$)"
lab var pay "Total weekly pay (\$)"
lab var productivity "Visit productivity"

*summary stats of wages by empl arrangement
preserve
bys st: sum pay wage prod_met productivity lemp

keep st lemp wage prod_met pay productivity

*contingent workers should have no productivity -> code to missing
replace prod_met = . if st > 3
replace productivity = . if st > 3
replace pay = . if pay==0

order st pay wage prod_met productivity

bysort st: outreg2 using `reg'/diff_hrchars.xls, replace sum(log) eqkeep(N mean) dec(2) tex label

restore

*within worker, is there any change in wage?
preserve
collapse (mean) mwage = wage (sd) sdwage = wage, by(payrollno st)
gen CoV = sdwage/mwage

bys payrollno: gen nst = _N

sort payrollno st
bys payrollno: gen f = _n==1
tab nst if f==1

bys st: sum mwage sdwage CoV
bys st: tab nst if f==1

keep st mwage sdwage CoV nst f
order st mwage sdwage CoV nst f
bysort st: outreg2 using `reg'/diff_hrchars3.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label

restore


*regression
loc file andiff_wage
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

loc xvar wage i.st
reg wage i.st, vce(cluster offid_nu)
`out' keep(`xvar')

reg wage i.st i.offid_nu, vce(cluster offid_nu)
`out' keep(`xvar') addtext(Office fixed effects, Yes)

loc xvar wage i.st tenure
reg wage i.st tenure i.offid_nu, vce(cluster offid_nu)
`out' keep(`xvar') addtext(Office fixed effects, Yes)

areg wage i.st tenure i.offid_nu, vce(cluster offid_nu) absorb(payrollno)
`out' keep(`xvar') addtext(Office fixed effects, Yes, Worker fixed effects, Yes)

*create tenure categories for 1 yr, 2 yr, etc.
forval x=1/4 {
    gen ten`x' = tenure*30 <= 365*`x'
    gen tenure_x_ten`x' = tenure*ten`x'
}

loc xvar wage i.st tenure ten1 ten2 ten3 ten4 tenure_x_ten* i.offid_nu
areg wage i.st tenure ten1 ten2 ten3 ten4 tenure_x_ten* i.offid_nu, vce(cluster offid_nu) absorb(payrollno)
`out' keep(`xvar') addtext(Office fixed effects, Yes, Worker fixed effects, Yes)

*tenure
use daily_workerpanel, clear

keep if majordisc=="SN"

*restrict to 67 offices
merge m:1 offid_nu using office67, keep(3) nogen

*aggregate to salaried, contingent-employee, contingent-contractors, and other categories
assert status!=""
tab payrollno if status==""
drop if status==""

*define contingent workers vs permanent ones
assert status!=""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

keep if monday >= mdy(2,27,2012) & monday <= mdy(9,28,2015)
/* keep if monday >= mdy(2,6,2012) & monday <= mdy(8,31,2015) */

*restrict to workers who quit & have employment start date non-missing
keep if esd!=. & etd!=.

*get first & last dates under each status
sort payrollno visitdate
capture drop ch
bys payrollno : gen ch = _n==1
bys payrollno : replace ch = ch + 1 if st!=st[_n-1] & _n > 1
/* list payrollno visitdate offid_nu st ch in 1/10 */

capture drop seq
bys payrollno : gen seq = ch + _n if ch==1
bys payrollno : replace seq = seq[_n-1] if st==st[_n-1]
count if seq==.

bys payrollno seq: gen days = _N

list payrollno visitdate offid_nu st ch seq days if payrollno=="381002"

collapse (max) lvd = visitdate (min) fvd = visitdate, by(payrollno esd st etd days)

bys payrollno: gen nn = _N

sort payrollno fvd lvd
list if nn==4

capture drop lemp
gen lemp = days
bys payrollno: replace lemp = lvd - esd + 1 if _n==1 & nn > 1

*actually, drop if esd < 2012
count if year(esd)<=2011
drop  if year(esd)<=2011

replace lemp = lemp/30
bys st: sum lemp

preserve
keep st lemp
bys st: sum lemp
lab var lemp "Tenure (months)"

bysort st: outreg2 using `reg'/diff_hrchars2.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label

restore


* turnover difference
insheet using learning_specialztn_wide2.csv, clear comma names
keep offid_nu pct_turnover? ym_visitd

*restrict to established offices
preserve
use panel_demandfl, clear
keep offid_nu established
duplicates drop
count
tempfile estab
save `estab'
restore

merge m:1 offid_nu using `estab'
keep if establish==1

sum pct_*

drop estab _merge
reshape long pct_turnover, i(offid_nu ym_visitd) j(st)

lab var pct_turnover "Monthly turnover rate (%)"

bysort st: outreg2 using `reg'/diff_hrchars3.xls, replace sum(log) eqkeep(mean sd) dec(2) tex label

*----------------------------------------
* difference in patient outcomes: (re)hospitalization & other functional outcomes, complaint counts
*----------------------------------------

*staff_complaint per office-worker-status
use staff_complaint, clear
tab status
tab jobcode
keep if jobcode=="LPN" | jobcode=="RN"

count if status==""
*3/4 obs have missing values in status

*get status for each worker
preserve
use visit_worker_chars, clear
keep workerID status offid_nu jobcode
duplicates drop
drop if status==""
tempfile status
save `status'
restore

count if offid_nu==.
foreach v of varlist jobcode status workerID {
    di "`v'-----"
    count if `v'==""
}
rename status status_miss

destring daysworked, replace i(",")
collapse (sum) complaint daysworked, by(workerID jobcode offid_nu status)
merge 1:m offid_nu workerID jobcode using `status', keep(1 3)

replace status_miss = status if status_miss=="" & status!=""

drop if status_mis==""
collapse (sum) complaint daysworked, by(workerID status_miss)

gen st = 1 if status=="VFT" | status=="VPB" | status=="VPC"
replace st = 2 if status=="VPD"
replace st = 3 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 4 if st==.
assert st!=.
lab define st4 1 "salaried" 2 "contingent-empl" 3 "contingent-ct" 4 "other"
lab values st st4

gen cd = complaint / daysworked
bys st: sum complaint daysworked cd

lab var complaint "Number of complaints"
lab var daysworked "Number of days worked"
lab var cd "Ratio of complaint counts to the number of days worked"

preserve
keep st complain daysworked cd

bysort st: outreg2 using "`reg'/complaintdiff_bylmix_`s'.xls", replace sum(log) eqkeep(N mean) tex label
restore

/* gen salaried = status=="VFT" | status=="VPB" | status=="VPC" | status=="SFT" | status=="SPB" | status=="SPC" | status=="EXEMPT" | status=="NON-EXEMPT-HR"
gen piecerate = 1 - salaried */
