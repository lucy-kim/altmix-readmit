*crrehosp_patsmpl_visit.do
*create the base sample of admission-episode-visit level data
*restrict to patients with prior hospitalization (for readmission outcome)
*exclude visits after first hospitalization date in the admission
*restrict to admissions for which I can identify whether the admission ended for one of the following reasons: death, hospitalized (first hosp), discharged (for this reason, restrict to episode starting on or before 8/9/2015)

loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'
loc mvar admissionclie

*create a patient-office-day level data where day is each day during her episode
use epi_visit, clear

*restrict to admission-episodes for which we can determine whether the admission ended for one of the following reasons: death, hospitalized (first hosp), discharged
*should restrict to episodes
tab dcdate_e
*since the number of obs with DC date after Sunday 8/9/2015 declines steeply, use 8/9 as the last available DC date
*for the episode that started on or before 8/9/2015 - 60 days, we know whether the episode ended with discharge; if it didn't end with a discharge, check if it had a hospitalization or death--if it didn't have either hosp or death, it should have a recertification afterwards. we don't want these admissions. then we are selectively dropping admissions that have multiple-episode admissions among admissions that have an episode starting on or after 8/9/2015 - 60 days. In the regression analysis, we only use 1-episode sample, so this is not a problem. But in the IV exogeneity analysis, we use all-admissions sample - for the IV analysis sample, just use the admissions whose last episode started on or before 8/9/2015 - 120 days (85% admissions have 1-2 episodes).

*restrict to episodes that started on or before 8/9/2015 - 59 days, i.e. 8/9/2015 is the 60th day of the episode
gen date = mdy(8,9,2015) - 59
count if epidate2 <= date
keep if epidate2 <= date
*0.3M obs dropped

sort `mvar' epiid visitd
list `mvar' epiid visitd epidate* hashosp firsthospdate death dcdate* in 1/30
*--------------------------
*drop visitdates after first hospitalization date in the admission
drop if visitdate > firsthospdate & firsthospdate!=.

*count if DC date is missing
count if dcdate_e==.

*drop if no hospitalization or death occurred
count if dcdate_e==. & hashosp==0 & death==0
drop if dcdate_e==. & hashosp==0 & death==0

*assert that the admission ended for one of the following reasons: death, hospitalized (first hosp), discharged
count if dcdate_e==.
assert hashosp==1 | death==1 if dcdate_e==.

*--------------------------
*create DC indicator
capture drop dced
gen dced = dcdate_e!=. & hashosp==0 & death==0
count if dced + hashosp + death!=1
egen rs = rowtotal(dced hashosp death)
tab rs
*2K obs have rs = 2 ; all of these died & had a hospitalization
tab death if rs==2
tab hashosp if rs==2
count if deathdate_e <= firsthospdate & rs==2
count if deathdate_e > firsthospdate & rs==2
*most of these 2K obs had the deaeth date after hospitalization

*create endstatus = 1,2,3 for discharge, hospitalized, died
gen endstatus = 1 if dced==1
*if the death occurred after the hosptialization, recode the hospitalization
replace endstatus = 2 if hashosp==1 & deathdate_e > firsthospdate
replace endstatus = 3 if death==1 & deathdate_e <= firsthospdate
assert endstatus!=.
lab define s3 1 "Discharged" 2 "Hospitalized" 3 "Died"
lab val endstatus s3
tab endstatus
lab var endstatus "End of admiss status: 1,2,3 for discharge, hospitalized, died"

count if payrollno==""
*0 obs
/*list workerID if payrollno==""*/
replace payrollno = workerID + "P" if payrollno=="" & workerID!=""
assert payrollno!=""

*tag each admission
sort `mvar' epidate2 visitdate
bys `mvar': gen i = _n==1
count if i==1
*202K admissions in total

*--------------------------

*what is the % patients who have > 1 episode?
sort `mvar' epiid visitdate

*since there are admissions for which I may not have all the episodes, use the # days under HHC to get the # episodes if the # days under HHC indicates a different # episodes than # episode IDs we have
capture drop daysinHH
bys `mvar': egen ffvd = min(visitd)
bys `mvar': egen llvd = max(visitd)
egen latest = rowmax(llvd dcdate2)
gen daysinHH = latest - ffvd + 1
tab daysinHH if i==1
drop if daysinHH > 60

capture drop nepi
gen nepi = ceil(daysinHH/60)
lab var nepi "Number of episodes under an admission until the endpoint"
assert nepi==1

drop rs date i ffvd llvd nepi

duplicates drop

*--------------------------
*drop episodes that had a visit on the day of readmission
sort epiid visitdate

gen hadvisit_onra = firsthospdate == visitdate & hashosp==1
bys epiid: egen max = max(hadvisit_onra)
drop if max==1
drop max hadvisit_onra
*--------------------------
bys epiid: gen i = _n==1
tab facility if i==1
/* *limit to patients with prior hosptialization
keep if facility=="Hosp" */

drop description tot_time
duplicates drop

compress
save single_epi_visitlvl, replace
