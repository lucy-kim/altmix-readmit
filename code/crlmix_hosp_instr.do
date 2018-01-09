*crlmix_hosp_instr.do
*for each patient, create instruments using 1) the absolute distance between patient and 1st, 2nd, 3rd closest FT and OC nurse in the month; 2) the absolute distance minus the serving distance in the month; 3) ZIP code level share of activity by any FT nurses or OC nurses

loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
set matsize 11000

*--------------------------------------------------------------------
* 1) the absolute distance between patient and 1st, 2nd, 3rd closest FT and OC nurse in the month
*--------------------------------------------------------------------

*use the raw / unrestricted visit-level data

*get visit-level data
use visit_worker_chars, clear

keep if discipline=="SN"
assert offid_nu!=.

*create month of the worker's visit date
capture drop yr
gen yr = year(visitdate)
gen mo = month(visitdate)
gen ym_visitd = ym(yr, mo)
format ym_visitd %tm

*get worker's ZIP code
merge m:1 payrollno workerID using staff_chars, keepusing(zipcode) keep(1 3) nogen
rename zipcode workerzip

*merge with episode-level data to get patient's ZIP code & unique client ID
merge m:1 epiid using client_chars, keepusing(patzip) keep(1 3) nogen
*1M have _m=1; 4M have _m=3

*convert to string var for patient & worker ZIP codes with leading 0's
foreach v of varlist patzip workerzip {
    capture destring `v', replace ig("-")
    gen str5 `v'2 = string(`v', "%05.0f")
    drop `v'
    rename `v'2 `v'
    drop if `v'=="." | `v'==""
}

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
drop status

tempfile tmp
save `tmp'

*get a list of active patients for each office-month
preserve
keep epiid ym_visitd patzip offid_nu
duplicates drop

tempfile active_pat
save `active_pat'
restore

*get a list of active nurses for each office-month
preserve
keep payrollno offid_nu ym_visitd workerzip st
duplicates drop

tempfile active_nurse
save `active_nurse'
restore


*--------------
*merge the patient episode-office-month level data by office-month with office-month-worker-status level data to get all possible combinations of episodes and workers
/* use `active_nurse', clear
joinby offid_nu ym_visitd using `active_pat'

compress
save ompw_new, replace

*compute distance between patient's ZIP code & office zip code for each office-month for every possible combination of patient and active nurse in that office-month
do /home/hcmg/kunhee/Labor/crzipdistance2 */


*--------------------------------------------------------
*who is the 1st, 2nd, 3rd closest FT and OC nurses for each patient, excluding the actual nurses who served her?

*get a list of actual nurses who served the patient episode from the visit data
*get visit-level data
use `tmp', clear
bys epiid: egen ym_fd = min(ym_visitd)

*keep only the actual nurses who visited the patient during the start-of-care month
keep if ym_fd==ym_visitd

keep epiid offid_nu ym_fd payrollno ym_visitd
duplicates drop

tempfile pat_actualnurse
save `pat_actualnurse'

*merge with data on the distance for every possible combination of ZIP codes by the patient ZIP-worker ZIP
use ompw_new, clear

*in this list of patient's all active nurses in each office-month, match with the patient's actual nurses to exclude them for each patient, by patient episode-month-office-payrollno
merge m:1 epiid offid_nu payrollno ym_visitd using `pat_actualnurse', keep(1 3)
*3 obs have _m=2 but these workers have missing values in status

bys epiid: egen mm = max(_merge)
assert mm==3
drop mm

*fill in missing values for start of care month in unmatched obs
gsort epiid -ym_fd
list epiid ym_fd _merge in 1/10
bys epiid: replace ym_fd = ym_fd[_n-1] if ym_fd >= .

*drop _merge==3 obs who are the nurses actually visited those patients
gen actual_nurse = _merge==3
drop _merge

tempfile active_nurse_except
save `active_nurse_except'


use `active_nurse_except', clear
drop if actual_nurse==1
drop actual_nurse

*merge with data on distances between ZIP codes
merge m:1 workerzip patzip using zipdistance, keep(1 3) nogen
drop zip1 zip2

*who are the most, 2nd, 3rd closest FT & OC nurses?
gsort epiid st mi_to_zcta5
loc v min1dist
bys epiid st: egen `v' = min(mi_to_zcta5)
/* bys offid_nu ym_fd epiid st: egen p10dist = pctile(mi_to_zcta5), p(10) */
assert mi_to_zcta5 ==. if  min1dist==.

loc d = 0.000001
loc cv closest1
gen `cv' = mi_to_zcta5 >= `v' - `d' & mi_to_zcta5 <= `v' + `d'
tab `cv'

*workers with 2nd closest distance
loc v min2dist
bys epiid st: egen `v' = min(mi_to_zcta5) if closest==0 & mi_to_zcta5 > min1dist

loc cv closest2
gen `cv' = mi_to_zcta5 >= `v' - `d' & mi_to_zcta5 <= `v' + `d'
tab `cv'

*workers with 3rd closest distance
loc v min3dist
bys epiid st: egen `v' = min(mi_to_zcta5) if closest1==0 & closest2==0 & mi_to_zcta5 > min2dist

loc cv closest3
gen `cv' = mi_to_zcta5 >= `v' - `d' & mi_to_zcta5 <= `v' + `d'
tab `cv'

*fill in missing values for 2nd min, 3rd min distance for each patient-status (no need for minimum)
forval x = 2/3 {
  loc v min`x'dist
  gsort epiid st -`v'
  bys epiid st: replace `v' = `v'[_n-1] if `v'>=.
}

*if for each patient-status, all the distances to different workers have missing values, then recode closest worker indicator to missing
gen miss = mi_to_zcta5==.
bys epiid st: gen nsum = sum(miss)
bys epiid st: gen nn = _N
forval x = 1/3 {
  replace closest`x' = . if nn==nsum
}
drop miss nsum nn

*average distance to all active nurses who didn't serve the patient
sum mi_to_zcta5
*15 miles across patient-workers
forval x = 1/3 {
  sum mi_to_zcta5 if closest`x' ==1
}
*6.1, 9.6, 11.4 miles

*how many workers are there in the closest ZIP code?
gsort epiid st mi_to
*list if epiid==36380
forval x = 1/3 {
  bys epiid st: egen nclosest`x' = sum(closest`x')
}
*list epiid st patzip workerzip mi_ min1-nclosest3 if epiid==36380

compress
save absdist_pom, replace

tempfile dist_ompw
save `dist_ompw'
*-------------


*--------------------------------------------------------------------
* 2) the absolute distance minus the serving distance in the month
*--------------------------------------------------------------------
*create worker-month-patient ZIP code level data on # visits, # episodes, distance from office to compute the serving distance

*use the raw / unrestricted visit-level data

*get visit-level data
use `tmp', clear

*tag visits
bys offid_nu payrollno epiid visitdate visittime: gen i = _n==1

*tag episodes: count episode once per worker-employment arrangement during the office-month
bys offid_nu payrollno st ym_visitd epiid : gen j = _n==1

collapse (sum) nv = i nepi = j, by(offid_nu payrollno ym_visitd st workerzip patzip)

sort offid_nu payrollno ym_visitd patzip
duplicates tag offid_nu payrollno ym_visitd patzip, gen(dup)
tab dup
*dup > 0 for 3% obs -> have multiple statuses
drop dup

tempfile wmsz
save `wmsz'

*merge with data on distances between any two ZIP codes
use `wmsz', clear
merge m:1 workerzip patzip using zipdistance, keep(1 3) nogen
drop zip1 zip2

bys payrollno ym_visitd st: egen tepi = sum(nepi)
bys payrollno ym_visitd st: egen tv = sum(nv)
gen shepi = nepi/tepi
gen shv = nv/tv
gen wgtdist_epi = mi_to*shepi
gen wgtdist_v = mi_to*shv

*aggregate across patient ZIP codes for patients they visit during month in each office under each status
*average distance to patient's ZIP code for each worker-office-month-status
collapse (sum) servdist_epi = wgtdist_epi servdist_v = wgtdist_v (mean) mi_to tepi tv nepi nv shepi shv, by(payrollno offid_nu ym_visitd st workerzip)

tempfile wms_servdist
save `wms_servdist'

*---------------
*calculate the actual distance between pat and worker minus the worker's serving distance for each office-month
use `dist_ompw', clear
rename mi_to_zcta5 dist_pat_nurse
capture drop _merge
merge m:1 offid_nu payrollno ym_visitd st workerzip using `wms_servdist', keep(1 3) keepusing(servdist* mi_to*) nogen
*all _m==3

gen reldist = dist_pat_nurse - servdist_v
rename mi_to avg_servdist_unadj

sort epiid st dist_pat_nurse

*who are the closest 3 FT, OC nurses by the relative distance?
*who are the most, 2nd, 3rd closest FT & OC nurses?
forval x=1/3 {
  rename min`x'dist min`x'dist_abs
  rename closest`x' closest`x'_abs
  rename nclosest`x' nclosest`x'_abs
}
rename reldist mi_to_zcta5


gsort epiid st mi_to_zcta5
loc v min1dist
bys epiid st: egen `v' = min(mi_to_zcta5)
assert mi_to_zcta5 ==. if  min1dist==.

loc d = 0.000001
loc cv closest1
gen `cv' = mi_to_zcta5 >= `v' - `d' & mi_to_zcta5 <= `v' + `d'
tab `cv'

*workers with 2nd closest distance
loc v min2dist
bys epiid st: egen `v' = min(mi_to_zcta5) if closest1==0 & mi_to_zcta5 > min1dist

loc cv closest2
gen `cv' = mi_to_zcta5 >= `v' - `d' & mi_to_zcta5 <= `v' + `d'
tab `cv'

*workers with 3rd closest distance
loc v min3dist
bys epiid st: egen `v' = min(mi_to_zcta5) if closest1==0 & closest2==0 & mi_to_zcta5 > min2dist

loc cv closest3
gen `cv' = mi_to_zcta5 >= `v' - `d' & mi_to_zcta5 <= `v' + `d'
tab `cv'

*fill in missing values for 2nd min, 3rd min distance for each patient-status (no need for minimum)
forval x = 2/3 {
  loc v min`x'dist
  gsort epiid st -`v'
  bys epiid st: replace `v' = `v'[_n-1] if `v'>=.
}

*if for each patient-status, all the distances to different workers have missing values, then recode closest worker indicator to missing
gen miss = mi_to_zcta5==.
bys epiid st: gen nsum = sum(miss)
bys epiid st: gen nn = _N
forval x = 1/3 {
  replace closest`x' = . if nn==nsum
}
drop miss nsum nn

*average distance to all active nurses who didn't serve the patient
sum mi_to_zcta5
*15 miles across patient-workers
forval x = 1/3 {
  sum mi_to_zcta5 if closest`x' ==1
}
*6.1, 9.6, 11.4 miles

*how many workers are there in the closest ZIP code?
gsort epiid st mi_to
*list if epiid==36380
forval x = 1/3 {
  bys epiid st: egen nclosest`x' = sum(closest`x')
}

forval x=1/3 {
  rename min`x'dist min`x'dist_rel
  rename closest`x' closest`x'_rel
  rename nclosest`x' nclosest`x'_rel
}
rename mi_to_zcta5 reldist

gsort epiid st reldist

compress
save distance, replace

use distance, clear
*keep FT or OC workers
keep if st==1 | st==4

*keep the first closest workers
gen good = 0
foreach v of varlist closest?* {
  replace good = 1 if `v'==1
}
keep if good==1
drop good

sort epiid st
list epiid st payrollno closest?* if epiid==6842

*separate data
keep epiid ym_fd offid_nu patzip st min* nclosest*
duplicates drop
reshape wide min* nclosest*, i(epiid ym_fd offid_nu patzip) j(st)

tempfile ins12
save `ins12'


*--------------------------------------------------------------------
* 3) ZIP code-start month level share of activity by any FT nurses or OC nurses for each patient (excluding activity devoted to the patient)
*--------------------------------------------------------------------

*visit level data
use `tmp', clear
gen i = 1
collapse (sum) tnv_byst = i, by(offid_nu patzip ym_visitd st)

tempfile tnv_byst
save `tnv_byst'

*create a balanced panel at the office-ZIP code-month level having 6 obs for work arrangements
use `tnv_byst', clear
keep offid_nu patzip ym_visitd
duplicates drop
expand 6
bys offid_nu patzip ym_visitd: gen st = _n
sort offid_nu patzip ym_visitd
lab val st stl
merge 1:1 offid_nu patzip ym_visitd st using `tnv_byst'
* should have no _m=2
foreach v of varlist tnv_byst {
  assert `v' !=. if _merge==3
  replace `v' = 0 if _merge==1
  assert `v' !=.
}
drop _merge

*total visits across all statuses for each office-pat ZIP code-month
bys offid_nu patzip ym_visitd: egen ttnv = sum(tnv_byst)

tempfile tnv_byst2
save `tnv_byst2'

use `tmp', clear
gen i = 1
collapse (sum) env_byst = i, by(offid_nu patzip ym_visitd st epiid)

*month of SOC
bys epiid: egen ym_fd = min(ym_visitd)
format ym_fd %tm
sort epiid ym_visitd
keep if ym_fd==ym_visitd

tempfile env_byst
save `env_byst'

*create a balanced panel at the episode level having 6 obs for work arrangements
use `env_byst', clear
keep epiid
duplicates drop
expand 6
bys epiid: gen st = _n
sort epiid
lab val st stl
merge 1:1 epiid st using `env_byst'
* should have no _m=2
foreach v of varlist env_byst {
  assert `v' !=. if _merge==3
  replace `v' = 0 if _merge==1
  assert `v' !=.
}
*fill in missing values
gsort epiid -_merge
foreach v of varlist offid_nu ym_visitd ym_fd  {
  bys epiid: replace `v' = `v'[_n-1] if `v' >= .
  assert `v'!=.
}
loc v patzip
bys epiid: replace `v' = `v'[_n-1] if `v' ==""
assert `v'!=""
drop _merge

*total visits across all statuses for each office-pat ZIP code-month-episode
bys offid_nu patzip ym_visitd epiid: egen tenv = sum(env_byst)

merge m:1 offid_nu patzip ym_visitd st using `tnv_byst2', keep(1 3) nogen
sort offid_nu patzip ym_visitd st epiid

*total visits to other patients across all status for each office-pat ZIP code-month
gen tnv_excl_act = ttnv - tenv

*total visits to other patients by each status for each office-pat ZIP code-month
gen nv_excl_act = tnv_byst - env_byst

* % visits in the office-month-pat ZIP code by FT (OC) nurses for all other patients
gen fr_nv_excl_act = nv_excl_act / tnv_excl_act
assert tnv_excl_act==0 if fr_nv_excl_act==.

bys epiid: egen ss = sum(fr_nv_excl_act)
tab ss
*ss = 0 or 1; 0 means there were no other patients getting visits than this episode
drop ss

keep epiid offid_nu patzip ym_fd st nv_excl_act tnv_excl_act fr_nv_excl_act
duplicates drop

*reshape wide
reshape wide fr_nv_excl_act nv_exc, i(offid_nu epiid patzip ym_fd tnv_exc) j(st)

/* *fill in missing values with 0 unless the denominator tnv_excl_act (total number of visits to ther patients in the office-ZIP-month) is 0
assert tnv_excl_act!=.
foreach v of varlist fr_nv_excl_act* {
  replace `v' = 0 if `v'==. & tnv_excl_act > 0
  assert `v' == . if tnv_excl_act==0
}
foreach v of varlist nv_exc* {
  replace `v' = 0 if `v'==. & tnv_excl_act > 0
} */

tempfile activity_share
save `activity_share'

*--------------------------------------------------------------------
* 4)  ZIP code-month level share of activity by any FT nurses or OC nurses for each patient
*--------------------------------------------------------------------

*visit level data
use `tmp', clear
gen i = 1
collapse (sum) tnv_byst = i, by(offid_nu patzip ym_visitd st)

*total visits across all statuses for each office-pat ZIP code-month
bys offid_nu patzip ym_visitd: egen ttnv = sum(tnv_byst)

* % visits in the office-month-pat ZIP code by FT (OC) nurses for all patients
gen fr_nv_st = tnv_byst/ttnv

keep offid_nu patzip ym_visitd st fr_nv_st tnv_byst ttnv
duplicates drop

*reshape wide
reshape wide fr_nv_st tnv_byst, i(offid_nu patzip ym_visitd ttnv) j(st)

*fill in missing values with 0 unless the denominator tnv_excl_act (total number of visits to ther patients in the office-ZIP-month) is 0
assert ttnv!=.
foreach v of varlist fr_nv_st* {
  replace `v' = 0 if `v'==. & ttnv > 0
  assert `v' == . if ttnv==0
}
foreach v of varlist tnv_byst* {
  replace `v' = 0 if `v'==. & ttnv > 0
}

destring patzip, replace

tempfile activity_share2
save `activity_share2'

*--------------------------------------------------------------------
* 5) ZIP code-start month level number of active FT nurses or OC nurses for each patient (excluding the actual nurses serving the patient)
*--------------------------------------------------------------------

use `active_nurse_except', clear
drop if actual_nurse==1
drop actual_nurse

*for each patient, count the number of active nurses in each employment status that didn't serve her in the SOC month
keep if ym_fd == ym_visitd
sort epiid st payrollno
gen i = 1
collapse (sum) nw_excl_act = i, by(epiid patzip ym_fd offid_nu st)

reshape wide nw_excl_act, i(epiid offid_nu patzip ym_fd) j(st)

foreach v of varlist nw_excl_act* {
  replace `v' = 0 if `v'==.
}
sum nw_excl_act*

tempfile active_nw_inzip
save `active_nw_inzip'

*--------------------------------------------------------------------
* for each patient episode ID, merge by start of care month-office, the instruments 1)--3) above
*--------------------------------------------------------------------

use `ins12', clear
merge 1:1 epiid offid_nu patzip ym_fd using `activity_share', keep(1 3) nogen
merge 1:1 epiid offid_nu patzip ym_fd using `active_nw_inzip', keep(1 3) nogen

destring patzip, replace

*merge episode-level data with ZIP code-month level share of FT nurse visits for all patients by episode's previous month
gen ym_visitd = ym_fd - 1
merge m:1 patzip offid_nu ym_visitd using `activity_share2', keep(1 3) nogen
rename ym_visitd ym_fd_lead

compress
save lmix_hosp_instr, replace
