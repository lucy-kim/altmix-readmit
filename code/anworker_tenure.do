*employment-related questions:
*1) what is the distribution of workers' tenure by discipline, by status, by office, or by state?
*2) what is the average time of retention for terminated workers?
*3) how many new workers start in each office-quarter? how many workers quit in each office-quarter? how many workers stay in each office-quarter?

set linesize 150
local path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

*for any office, use the first (last) day of working as employment start (end) date
*if the worker didn't work for 90 days in a row, then the person quit

*for each worker, tag if the worker is inactive (i.e. doesn't provide any visit) for X days
use visit_worker_chars, clear

assert visitdate!=.
assert payrollno!=""
assert offid_nu!=.

*for worker, create active_q42015 = 1 if the worker still provides some visits in Dec of 2015
gen i = 1
bys payrollno visitdate: egen tv = sum(i)
gen x = tv > 0 & visitdate > mdy(9,30,2015)
bys payrollno: egen max = max(x)
gen active_q42015 = max > 0
drop i tv x max

tempfile t
save `t'

*get 2011 visit data to get an indicator for working before 2012 for each workerID in each office?
use visit_allqtrs, clear
keep if yr==2011
keep workerID epiid
duplicates drop

*get office ID
merge m:1 epiid using episode_visitcnt_offid, keepusing(offid_nu) keep(1 3) nogen

assert workerID!=""
assert offid_nu!=.
keep workerID offid_nu
duplicates drop

*merge with workerID-payrollno xwalk
merge m:1 workerID using staff_chars, keep(1 3) keepusing(payrollno)
*only 6 _m=1; 2K _m=3
replace payrollno = workerID + "P" if _m==1
drop _merge
gen active_q42011 = 1

*drop office ID
drop offid_nu
duplicates drop

tempfile active_q42011
save `active_q42011'

*merge the visit-level data with the info on whether the worker is active in 2011
use `t', clear
merge m:1 workerID payrollno using `active_q42011', keep(1 3)
replace active_q42011 = 0 if _merge==1
assert active_q42011!=.
drop _merge

tempfile t
save `t'
*-------------------
*identify whether a worker is a new hire, separated
use `t', clear

assert payrollno!=""
bys payrollno : egen fvd = min(visitd)
bys payrollno : egen lvd = max(visitd)
format fvd %d
format lvd %d
lab var fvd "First ever visit for payrollno during 1/1/2012-12/31/2015"
lab var lvd "Last ever visit for payrollno during 1/1/2012-12/31/2015"

*worker = new hire if the worker was not active during 2011
gen newhire_cs = active_q42011==0 & fvd >= mdy(1,1,2012)
tab newhire
lab var newhire "=1 if not active during 2011 (active_q42011==0) & 1st visit >= 1/1/2012"

*active_q42015 = 1 means the worker is still working on or after 10/1/2015

*worker = separated if not working for any offices in Oct-Dec 2015
capture drop attrited_cs
gen attrited_cs = active_q42015==0
*gen attrited_cs = lvd < mdy(10,5,2015) & active_q42015==0
tab attrited
lab var attrited "=1 if not active during Q4 2015"

*get the employment start & termination (if applicable) dates
gen esd = fvd if newhire==1
gen etd = lvd if attrited==1
lab var esd "employment start date"
lab var etd "employment termination date"
format esd %d
format etd %d

tempfile beforedelete
save `beforedelete'


*can I get employment start date for non-new hires?
*use wk1_emplst (First week recorded in the historical empl status data for the worker) & date_hired_e & empstartdate
use `beforedelete', clear

*merge w/ cross-sectional worker chars data
merge m:1 workerID payrollno using staff_chars, keep(1 3) nogen keepusing(date_hired empstartdate)

list payrollno workerID visitd status esd etd fvd lvd newhire attrited wk1_emplst date_hired empstartdate if payrollno=="Nov-36" & visitd==mdy(11,2,2012)

*get number of visits on each day for each worker
gen i = 1
collapse (sum) nv = i, by(payrollno visitd status esd etd fvd lvd newhire attrited wk1_emplst date_hired empstartdate)
duplicates tag payrollno visitd, gen(dd)
tab dd
*8 obs have dd ==1 - these guys had multiple worker IDs for a single payrollno - drop these
bys payrollno: egen bad = max(dd)
drop if bad==1
drop bad dd

*if not new hire, get an employment start date using other sources: from historical empl status data
preserve
keep if newhire==0

*since HCHB empl start date = 1/1/2000 is not accurate, don't assign these dates
tab empstartdate, sort
replace empstartdate = . if empstart==mdy(1,1,2000)

capture drop earliest
egen earliest = rowmin(date_hired wk1_emplst empstartdate fvd)
count if earliest==.
replace earliest = . if earliest!=. & earliest > fvd
assert earliest <= fvd if earliest!=.

replace esd = earliest if earliest!=. & esd==.
count if esd==.
drop earliest

tempfile notnewhire
save `notnewhire'
restore

drop if newhire==0
append using `notnewhire'
drop if status==""

*restrict to workers who have non-missing empl start & end dates
keep if esd!=. & etd!=.

tempfile tmp
save `tmp'

*create a panel for each worker
use `tmp', clear
assert fvd >= esd
assert lvd <= etd
keep payrollno fvd lvd
duplicates drop
gen gap = lvd - fvd + 1
expand gap
sort payrollno
bys payrollno: gen visitdate_e = fvd + _n - 1
sort payrollno visitdate
bys payrollno: assert visitdate == lvd if _n==_N
drop gap

merge 1:1 payrollno visitdate fvd lvd using `tmp', keepusing(esd etd nv newhire_cs status)
replace nv = 0 if _merge==1

*fill in missing values
sort payrollno visitdate
foreach v of varlist esd etd newhire_cs {
  bys payrollno: replace `v' = `v'[_n-1] if `v' >=.
  assert `v'!=.
}
foreach v of varlist status {
  bys payrollno: replace `v' = `v'[_n-1] if `v' ==""
  assert `v'!=""
}
drop _merge

*if the worker didn't work for 90 days in a row, then the person quit

*create absent indicator
gen absent = nv==0

*tag whenever absent changes from 1 to 0 or vice versa for each worker (not worker-office)
sort payrollno visitdate
capture drop ch
bys payrollno : gen ch = _n==1
bys payrollno : replace ch = ch + 1 if absent!=absent[_n-1] & _n > 1

capture drop seq
bys payrollno : gen seq = ch + _n if ch==1
bys payrollno : replace seq = seq[_n-1] if absent==absent[_n-1]
assert seq!=.

*length of period during visit block
bys payrollno seq: gen len = _N

*create long break indicator
gen longbreak = len > 90 & absent==1

*if the worker has a break between visits for longer than 90 days, consider the worker as having attrited (i.e. already coded leave=1 on those days); but I should recode the employment start & termination date for this period differently

*tag whenever the long break indicator switches
sort payrollno visitdate
capture drop ch
bys payrollno : gen ch = _n==1
bys payrollno : replace ch = ch + 1 if longbreak!=longbreak[_n-1] & _n > 1

capture drop seq2
bys payrollno : gen seq2 = ch + _n if ch==1
bys payrollno : replace seq2 = seq2[_n-1] if longbreak==longbreak[_n-1]
assert seq2!=.

sort payrollno seq2 visitd
bys payrollno seq2: egen fds = min(visitd)
bys payrollno seq2: egen lds = max(visitd)

*if the next day is the start of long break period, then recode the empl start & end date for the preceding block to the first & last day of the block, respectively
bys payrollno: gen etd2 = lds if longbreak[_n+1]==1
bys payrollno: gen esd2 = fds if longbreak[_n+1]==1

*if the previous day is the end of a long break period, then recode the empl start & end date for the block to the first & last day of the block, respectively
bys payrollno: replace etd2 = lds if longbreak[_n-1]==1
bys payrollno: replace esd2 = fds if longbreak[_n-1]==1

foreach v of varlist etd2 esd2 visitd fds lds {
  format `v' %d
}

drop if longbreak==1
gsort payrollno seq2 -etd2
foreach v of varlist etd2 esd2 {
  bys payrollno seq2: replace `v' = `v'[_n-1] if `v' >= .
}
sort payrollno visitd

*for workers who didn't have any long break periods, then use the original esd & etd
foreach v in "esd" "etd" {
  replace `v'2 = `v' if `v'2==.
  assert `v'2!=.
  assert `v'!=.
}
*restrict to worker-block obs
preserve
keep payrollno seq2 status esd2 etd2
duplicates drop
gen len_mo = (etd2 - esd2 + 1)/30

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

bys st: sum len_mo

keep st len_mo
lab var len_mo "Length of employment (months)"
bysort st: outreg2 using `reg'/lemp.xls, replace sum(log) eqkeep(N mean) dec(2) tex label



*---------- OLD code below

*export summary stats of length of employment by discipline
use worker_tenure, clear
keep payrollno majordisc lemp
keep if lemp!=.
duplicates drop
drop if major==""

*convert to # months
replace lemp = lemp*7
replace lemp = lemp/30

drop if majordisc=="FS" | majordisc=="RD"
collapse (mean) Mean = lemp (sd) SD = lemp (min) Min = lemp (p5) P5= lemp (p25) P25 = lemp (p50) P50 = lemp (p75) P75 = lemp (p95) P95 = lemp (max) Max = lemp, by(majordisc)
gsort -major
gen a = _n
replace a = 4.5 if major=="ST"
sort a
drop a
outsheet using `reg'/lemp.csv, comma names replace

/*
foreach dd in "SN" "PT" "ST" "OT" "HHA" "MSW" {
    hist lemp if discipline=="`dd'", name(`dd') title(`dd')
}
graph combine SN PT ST OT HHA MSW
graph save `gph'/lemp.gph, replace asis*/

*------------------------------
*using the worker-office-day level data, consider worker-office-status as a unit; so same worker-office w/ different statuses are different units

use daily_workerpanel, clear

keep if majordisc=="SN"

gen i = 1
collapse (sum) ndays = i nworkdays = worked nabsentdays = absent work_manyoffice, by(payrollno offid_nu majordisc status officew* fieldw* contractor salaried piecerate)
sum ndays nworkdays nabsentdays work_manyoffice
foreach v of varlist nworkdays nabsentdays work_manyoffice {
    gen pct_`v' = 100*`v'/ndays
}
sum ndays pct_* if fieldw_sa==1
sum ndays pct_* if fieldw_pr==1 & contractor==0
sum ndays pct_* if contractor==1

*attrition
use daily_workerpanel, clear

keep if majordisc=="SN"

keep payrollno offid_nu majordisc status officew* fieldw* contractor salaried piecerate fired attrited
duplicates drop

tab fired if fieldw_sa==1
tab fired if fieldw_pr==1 & contractor==0
tab fired if contractor==0

gen quit = attrited==1 & fired==0
replace quit = . if fired==.
tab quit if fieldw_sa==1
tab quit if fieldw_pr==1 & contractor==0
tab quit if contractor==0




*how many days of employment for each worker-office-discipline-status?
bys payrollno offid_nu majordisc status: gen ndays = _N

*how many days of working?
bys payrollno offid_nu majordisc status: egen nworkdays = sum(worked)

*how many days of absence?
bys payrollno offid_nu majordisc status: egen nabsentdays = sum(absence)


*what is the length of employment by status-discipline?
/*keep payrollno majordisc lemp status
keep if lemp!=.
duplicates drop
drop if major==""

*convert to # months
replace lemp = lemp*7
replace lemp = lemp/30

drop if majordisc=="FS" | majordisc=="RD"
collapse (mean) Mean = lemp (sd) SD = lemp (min) Min = lemp (p5) P5= lemp (p25) P25 = lemp (p50) P50 = lemp (p75) P75 = lemp (p95) P95 = lemp (max) Max = lemp, by(majordisc)*/

*what is the attrition rate (quit & layoff)?
gen attrit = 1 if attrited==1 &




*---------- delete the code below?

use pay_byworker_byweek, clear

*convert effective from date to stata data format
split effective, p("/")
replace effectivefrom3 = "20" + effectivefrom3
replace effectivefrom3 = "" if effectivefrom3=="20"
destring effectivefrom?, replace
gen empstartdate = mdy(effectivefrom1, effectivefrom2, effectivefrom3)
format empstartdate %d
drop effective*
lab var empstartdate "Date hired; year=2000 means employed before 2011"

list workerID *date* in 1/10
*date_hired is not equal to empstartdate -> trust date_hired more
codebook date_hired

*for each worker, what is the first date she's paid
sort workerID startd
bys workerID: gen a = startdate if _n==1
bys workerID: gen b = startdate if _n==_N
bys workerID: egen firstweek = max(a)
bys workerID: egen lastweek = max(b)
list workerID firstweek lastweek in 1/10
format firstweek %d
format lastweek %d
drop a b

*------------------------------------------------------------------
*what is the tenure for each worker?
*------------------------------------------------------------------
*first correct the employment start date - choose the earlier date
gen hireddate2 = min(empstart, date_hired)
format hireddate2 %d

/* list workerID firstweek lastweek date_hired date_term in 1/10 */
capture drop tenure
gen tenure = ceil( (startdate - hireddate2)/7 )
*if hireddate2 is missing, use firstweek as the hireddate2
replace tenure = ceil( (startdate - firstweek)/7 ) if hireddate2 == .
replace tenure = ceil( (startdate - firstweek)/7 ) if tenure < 0

preserve
keep tenure primary_jobcode
gen tenyr = tenure/52
compress
saveold tenure, replace v(12)
restore
*------------------------------------------------------------------
*how many workers does each home office hire?
*------------------------------------------------------------------
