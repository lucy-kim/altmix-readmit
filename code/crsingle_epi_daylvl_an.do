*fine-tune the sample for regression analysis by applying sample restriction rules & filling in missing values in patient characteristics

loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'

use single_epi_daylvl, clear

* restrict to patients with prior hospitalization
keep if facility=="Hosp"

* for episode-level health status & other patient characteristics, match again at the episode level to avoid filling in missing values manually
keep epiid visitdate_e offid_nu admissionclientsocid epidate2 endstatus-ho_oth payrollno jobcode monday
drop i x miss
duplicates drop

*get episode-level patient characteristics data
preserve
use client_chars2, clear
sort admissionclientsocid epidate2
bys admissionclientsocid: keep if _n==1

keep admissionclientsocid npayer-elixsum riskhosp_fall-ADLhelp_rarely age female
drop epi* fvd lvd
duplicates drop
duplicates tag admissionclientsocid, gen(dd)
assert dd==0
drop dd
tempfile clientchars
save `clientchars'
restore

*fill in missing values in admission ID before merging by it
gsort epiid -admissionclientsocid
bys epiid: replace admissionclientsocid = admissionclientsocid[_n-1] if admissionclientsocid >= .
assert admissionclientsocid!=.

merge m:1 admissionclientsocid using `clientchars', keep(1 3) nogen
*all merged

*fill in missing values in the inpatient DC date before creating the 30-day readmission outcome
preserve
keep epiid
duplicates drop
assert epiid!=.
merge 1:m epiid using single_epi_visitlvl, keepusing(admissionclie clientid socdate_e epiid) keep(1 3) nogen
duplicates drop
merge 1:m clientid socdate using inpat_dcdate, keep(1 3)
duplicates tag epiid, gen(dup)
tab dup
gen bad = inpat_dcd > socdate
drop if bad ==1
duplicates drop
drop dup
duplicates tag epiid, gen(dup)
tab dup
bys epiid: egen dcd = min(inpat_dcd)
drop if dup > 0 & inpat_dcd!=dcd
keep epiid inpat_dcd
duplicates drop
tempfile inpat_dcd
save `inpat_dcd'
restore

drop inpat_dcdate_e0
merge m:1 epiid using `inpat_dcd', nogen keep(1 3)

*Create readmission ever & 30-day readmission  outcome
sort epiid visitdate
gen hospoccur = visitdate==firsthospdate
tab hospoccur

gen days2rehosp_hdc = firsthospdate - inpat_dcdate
gen hospoccur30 = hospoccur==1 & days2rehosp_hdc <= 30 & inpat_dcdate!=.
tab hospoccur30
drop days2rehosp_hdc

*Create home health day index
sort epiid visitdate
bys epiid: gen hhday = _n
*some episodes have hhday > 60
bys epiid: egen max = max(hhday)
count if max > 60
drop if max > 60
assert hhday >=1 & hhday <= 60
drop max
/* tab epiid if hhday < 1 | hhday > 60
list epiid visitdate hhday daysinHH firsthospdate hospoccur hadSNvisit if epiid==510224 */

*create day of week & year-month of each day
gen dow = dow(visitdate)
gen yr = year(visitdate)
gen mo = month(visitdate)
gen ym = ym(yr, mo)
format ym %tm

*fill in missing values in the end status
loc v endstatus
gsort epiid -`v'
bys epiid: replace `v' = `v'[_n-1] if `v' >=.

tempfile tmp
save `tmp'
*-----------
*merge with office-day level data on # active SNs, # ongoing episode
preserve
use daily_officepanel, clear
collapse (sum) nw_active_SN nv_SN (mean) allepi newepi, by(offid_nu visitdate)

keep offid_nu visitd nw_active_SN allepi nv_SN newepi
tempfile office
save `office'
restore

merge m:1 offid_nu visitdate using `office', keep(1 3) nogen

foreach v of varlist nw_active_SN allepi {
  gen ln_`v' = ln(`v')
}

*keep if age >= 65
keep if age >=65

*create 5-year age bins
gen agecat = .
forval x=1/6 {
  loc x0 = 65 + (`x'-1)*5
  loc x1 =`x0'+4
  replace agecat = `x' if age >= `x0' & age <= `x1'
  *gen age_`x0'_`x1' = age >= `x0' & age <= `x1'
}
replace agecat = 7 if age >= 95
assert agecat!=.

compress
save single_epi_daylvl_an, replace

*may drop AIDS, blood loss anemia, cancer later
