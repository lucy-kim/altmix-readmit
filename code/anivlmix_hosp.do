*IV regression of the effect of fraction of visits by contingent nurses on the probability of hospitalization by facility type patient was in before home health care
loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'
loc nepi_byo_thresh 30
loc nv_SN_thresh 20
loc mvar admissionclie
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
set matsize 11000
capture ssc install reghdfe

*controlling vars

loc ages age??_lt*
*omitted variable = age >= 95
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc demog `ages' female white `ins' noassist livealone
loc comorbid charlindex `overallst'  `hrfactor' `priorcond'
*ynch*
loc scale allepi nw_SN

*table macros
loc hosprisk1 .
loc hosprisk2 Yes
loc hosprisk3 Yes
loc hosprisk4 Yes
loc demog1 .
loc demog2 .
loc demog3 Yes
loc demog4 Yes
loc comb1 .
loc comb2 .
loc comb3 .
loc comb4 Yes

use lmix_onhosp_new2, clear
*use ivlmix_onquality, clear

*merge with instrument data
merge 1:1 epiid offid_nu patzip ym_fd using lmix_hosp_instr, keep(3) nogen

*drop missing values in one comorbidity var to make the samples uniform
drop if priorcond_cath==.
drop if riskhosp_oth==.
drop if white==.

lab var nv_d_PT "Number of physical therapy visits"
lab var nv_d_ST "Number of speech therapy visits"
lab var nv_d_OT "Number of occupational therapy visits"
lab var nv_d_MSW "Number of medical social service visits"
lab var nv_d_HHA "Number of home health aide visits"

*create age brackets
forval x = 1/6 {
  loc a1 = 65 + 5*(`x'-1)
  loc a2 = 65 + 5*`x'
  capture drop age`a1'_lt`a2'
  gen age`a1'_lt`a2' = age >= `a1' & age < `a2'
}
gen age95 = age >= 95
egen aa = rowtotal(age*_* age95)
assert aa==1
drop aa

*label variables
label var ynch1 "Acute myocardial infarction (AMI)"
label var ynch2 "Congestive heart failure (CHF)"
label var ynch3 "Peripheral vascular disease (PVD)"
label var ynch4 "Cerebrovascular disease (CEVD)"
label var ynch5 "Dementia"
label var ynch6 "Chronic pulmonary disease (COPD)"
label var ynch7 "Rheumatic disease"
label var ynch8 "Peptic ulcer disease"
label var ynch9 "Mild liver disease"
label var ynch10 "Diabetes"
label var ynch11 "Diabetes + Complications"
label var ynch12 "Hemiplegia or paraplegia (HP/PAPL)"
label var ynch13 "Renal disease"
label var ynch14 "Cancer"
label var ynch15 "Moderate/severe liver disease"
label var ynch16 "Metastatic cancer"
label var ynch17 "AIDS/HIV"

lab var overallst_vbad "Overall status having serious progressive conditions (Very bad)"
lab var overallst_bad "Overall status likely to remain in fragile health (Bad)"
lab var overallst_tempbad "Overall status temporarily facing high health risks (Less bad)"
lab var hrfactor_alcohol "High risk factor: Alcohol dependency"
lab var hrfactor_drug "High risk factor: Drug dependency"
lab var hrfactor_smoke "High risk factor: Heavy smoking"
lab var hrfactor_obese "High risk factor: Obesity"
lab var priorcond_disrupt "Pre-home health condition: Disruptive behavior"
lab var priorcond_impdm "Pre-home health condition: Impaired decision-making"
lab var priorcond_cath "Pre-home health condition: Indwelling/Suprapublic catheter"
lab var priorcond_pain "Pre-home health condition: Intractable pain"
lab var priorcond_memloss "Pre-home health condition: Memory loss"
lab var priorcond_incontn "Pre-home health condition: Urinary incontinence"
lab var age "Age"
lab var female "Female"
lab var white "White"
lab var ma_visit "Enrolled in per-visit paying Medicare Advantage"
lab var ma_epi "Enrolled in per-episode paying Medicare Advantage"
lab var dual "Dual eligible"
lab var noassist "No assistance available"
lab var livealone "Living alone"
lab var riskhosp_fall "Risk for hospitalization: History of 2+ falls"
lab var riskhosp_manyhosp "Risk for hospitalization: 2+ hospitalizations"
lab var riskhosp_mental "Risk for hospitalization: Recent mental decline"
lab var riskhosp_ge5med "Risk for hospitalization: Take 5+ medications"
lab var riskhosp_oth "Risk for hospitalization: Other"
lab var charlindex "Charlson Comorbidity Index"

lab var allepi_od "Mean daily number of episodes in the office"
lab var tnw_SN_od "Mean daily number of active nurses in the office"
lab var fnw_stSN1_od "Mean daily proportion of full-time nurses in the office"
lab var hashosp "Indicator for hospital readmission"
lab var nhandoffs "Number of nurse handoffs"
lab var nv_d_SN "Number of nurse visits"

tempfile regdata
save `regdata'

*Get time-variant firm age variable
use weekly_officepanel.dta, clear
gen stmo = ym(year(realstartwk), month(realstartwk))
gen monmo = ym(year(monday), month(monday))
gen age_mo = monmo - stmo + 1

keep offid_nu age_mo monday
duplicates drop
assert age_mo!=.
count
tempfile age
save `age'

*-------------------------
*construct IV analysis data
use `regdata', clear

*merge with the firm age in months for each office-week of the end of care
merge m:1 offid_nu monday using `age', keep(1 3)
format ld %d
gen monmo = ym(year(monday), month(monday))

sort offid_nu monday ld
forval x=1/2 {
  bys offid_nu: replace age_mo = age_mo[_n-1] + 1 if age_mo >=. & monmo[_n-1]==monmo - 1
  bys offid_nu: replace age_mo = age_mo[_n-1] if age_mo >=. & monmo[_n-1]==monmo
}

gsort offid_nu -monday -ld
forval x=1/2 {
  bys offid_nu: replace age_mo = age_mo[_n-1] - 1 if age_mo >=. & monmo[_n-1] - 1 ==monmo
  bys offid_nu: replace age_mo = age_mo[_n-1] if age_mo >=. & monmo[_n-1]==monmo
}
drop if age_mo==0
drop monmo

*list offid ld monday monmo age_mo if _merge==1

*create week of year dummies
tab monday
preserve
keep monday
gen yr = year(monday)
duplicates drop
sort yr monday
assert monday==monday[_n-1] + 7 if _n > 1
bys yr: gen wkofyr = _n
tempfile wkofyr
save `wkofyr'
restore

merge m:1 monday using `wkofyr', nogen

*sum % visits by contract & on-call workers
gen fnv_st_pr = fnv_st4 + fnv_st5

compress
save an_new, replace

*------------------------------------------------
*restrict to non-senior living offices
use office, clear
keep offid_nu officefullname startuptype
gen seniorliv = regexm(officefullname, "Senior Living")
drop officefullname
duplicates drop

merge 1:m offid_nu using an_new, keep(2 3) nogen
keep if seniorliv==0

*drop if the number of nurse visits in an episode is < 2 since they can't have a labor mix
drop if nv_d_SN < 2

capture drop ss
bys patzip offid_nu: egen ss = sum(1)
count if ss < 50
drop if ss < 50
drop ss

/* sort offid_nu patzip epiid
bys offid_nu patzip: gen i = _n==1
bys offid_nu: egen cntzip = sum(i)
tab offid_nu, summarize(cntzip)
drop if cntzip==1 */

gen rate_handoff = nhandoffs/nv_d_SN

loc ss 1

*drop if the number of SN visits during episode is > 99th percentile (18)
sum nv_d_SN, de
drop if nv_d_SN > `r(p99)'

lab var rate_handoff "Ratio of handoffs to nurse visits"
lab var nv_d_SN "Number of nurse visits"
lab var min1dist_abs1 "Distance to the closest FT (absolute)"
lab var min2dist_abs1 "Distance to the 2nd closest FT (absolute)"
lab var min3dist_abs1 "Distance to the 3rd closest FT (absolute)"
lab var min1dist_rel1 "Distance to the closest FT (relative to serving distance)"
lab var min2dist_rel1 "Distance to the 2nd closest FT (relative to serving distance)"
lab var min3dist_rel1 "Distance to the 3rd closest FT (relative to serving distance)"
lab var nclosest1_abs1 "Number of preoccupied nearest full-time nurses"
lab var nclosest2_abs1 "Number of 2nd closest FT (by abs dist)"
lab var nclosest3_abs1 "Number of 3rd closest FT (by abs dist)"
lab var nclosest1_rel1 "Number of closest FT (by rel dist)"
lab var nclosest2_rel1 "Number of 2nd closest FT (by rel dist)"
lab var nclosest3_rel1 "Number of 3rd closest FT (by rel dist)"
lab var fnv_st1 "Proportion of full-time nurse visits"
lab var fnv_st4 "Proportion of on-call nurse visits"
lab var fr_nv_excl_act1 "Full-time nurses' activity share"
lab var fr_nv_excl_act2 "Part-time nurses' activity share"
lab var fr_nv_excl_act3 "Part-time (without benefits) nurses' activity share"
lab var fr_nv_excl_act4 "On-call nurses' activity share"
lab var fr_nv_excl_act5 "Contractor nurses' activity share"
lab var fr_nv_excl_act6 "Other/Office nurses' activity share"

*restrict to data points used for regression analysis
loc officechars allepi_od tnw_SN_od fnw_stSN`ss'_od
loc sp1 i.dow i.wkofyr i.yr `officechars'
reghdfe hashosp fr_nv_excl_act* nclosest1_abs1 `sp1' fnv_st`ss' rate_handoff nv_d_SN, vce(cluster patzip offid_nu) absorb(offid_nu patzip)

capture drop insmpl
gen insmpl = e(sample)
keep if insmpl==1
drop insmpl

*merge with episode-day level data to get average gap in days between 2 consecutive visits
tempfile an2
save `an2'

use `an2', clear
merge 1:m epiid using handoff_pd, keep(3) nogen keepusing(vseq hadSNvisit daysnovSN cum_nw_seen)
* drop 1 obs with _m==1

*get total number of unique nurses during the care
bys epiid: egen n_uniqnurse = max(cum_nw_seen)

*control for frequency of visits (i.e. plan of care) using avg time b/w visits during the episode for each patient
*create a new visit block that starts on the day after a visit & ends on the day the next visit is provided
gen vseq2 = vseq
replace vseq2 = vseq2 - 1 if hadSNvisit==1
bys epiid vseq2: egen gap = max(daysnovSN)

preserve
keep epiid vseq2 gap
duplicates drop
bys epiid: egen avgvgap = mean(gap)
drop gap
tempfile meangap_byepi
save `meangap_byepi'
restore

merge m:1 epiid vseq2 using `meangap_byepi', nogen
drop vseq* daysnovSN gap cum_nw_seen hadSNvisit
duplicates drop

lab var avgvgap "Mean number of days between two consecutive visits"
lab var n_uniqnurse "Total number of unique nurses seen"
lab var charlindex "Charlson comorbidity index"

*restrict to offices that have multiple ZIP codes within the same month
sort offid_nu ym_fd patzip epiid
capture drop i
bys offid_nu ym_fd patzip: gen i = _n==1
bys offid_nu ym_fd: egen nzipcode = sum(i)
drop if nzipcode==1
drop nzipcode

compress
save anivlmis_hosp, replace

*------------------------------------------------
*descriptive analysis

use anivlmis_hosp.dta, clear

corr nclosest1_abs1 fnv_st1

gen lnnclosest1_abs1 = log(nclosest1_abs1)
hist lnnclosest1_abs1

binscatter fnv_st1 nclosest1_abs1
graph export `gph'/test.eps, replace

*-------------------------------------------------------------------
* patient's severity by the proportion of full-time nurse visits
*-------------------------------------------------------------------
use anivlmis_hosp.dta, clear

*export for the exploration of variation in endogenous variables
loc ss 1
keep epiid fnv_st* hashosp
*offid_nu patzip
duplicates drop
outsheet using endvar.csv, replace comma names

*how much variation is there across patients the labor mix exposure?
sum fnv_st`ss', de
tab fnv_st`ss'
hist fnv_st`ss', frac xti("Fraction of visits by full-time nurses")
graph export `gph'/fnv_st`ss'.eps, replace

*----------------------
* divide patients into 4 groups using cutoffs based on the % FTN visits and get mean values for patient severity characteristics & other endogenous variables
use anivlmis_hosp.dta, clear

loc ss 1
capture drop lmix
sum fnv_st`ss', de
loc th `r(p50)'
gen lmix = 1 if fnv_st`ss'==0
replace lmix = 2 if fnv_st`ss' > 0 & fnv_st`ss' < `th'
/* replace lmix = 3 if fnv_st`ss' > 0.25 & fnv_st`ss' <= 0.50 */
replace lmix = 3 if fnv_st`ss' >= `th' & fnv_st`ss' < 1
/* replace lmix = 5 if fnv_st`ss' > 0.75 & fnv_st`ss' < 1 */
replace lmix = 4 if fnv_st`ss'==1
tab lmix
bys lmix: sum fnv_st`ss'

*calculate number of days during the care
gen length = ld - fd + 1
lab var length "Length of care (in days)"

keep epiid lmix fnv_st`ss' rate_handoff nv_d_SN nv_d_PT nv_d_OT nv_d_ST nv_d_HHA length avgvgap n_uniqnurse `riskhosp' age female white ma_visit ma_epi dual noassist livealone `comorbid' hashosp
duplicates drop
order epiid lmix fnv_st`ss' rate_handoff nv_d_SN avgvgap `riskhosp' age female white ma_visit ma_epi dual noassist livealone `comorbid' hashosp length n_uniqnurse nv_d_PT nv_d_OT nv_d_ST nv_d_HHA

bys lmix: outreg2 using `reg'/epi_summ_lmix.xls, tex replace sum(log) label eqkeep(N mean) dec(2) fmt(fc)

*----------------------------------------------------------
* describe activity share variation across ZIP codes wihtin the firm-month or across months within firm-ZIP code pairs
*----------------------------------------------------------
*A. Across ZIP Codes within Firms
use anivlmis_hosp.dta, clear

keep epiid offid_nu patzip ym_fd fr_nv_excl_act* fnv_st1
duplicates drop
duplicates tag, gen(dd)
assert dd==0
drop dd

*first, on avg, how many ZIP codes does a firm-month serve?
preserve
keep offid_nu ym_fd patzip
duplicates drop
bys offid_nu ym_fd: gen nzipcode = _N
keep offid_nu ym_fd nzipcode
duplicates drop
sum nzipcode, de
restore

*second, the mean HHI of episodes within the firm-month pairs
bys offid_nu ym_fd patzip: gen n_omz = _N
bys offid_nu ym_fd: gen n_om = _N
gen sh_omz = n_omz / n_om

preserve
keep offid_nu ym_fd patzip sh_omz
duplicates drop
gen sh_omz_sq = sh_omz * sh_omz

collapse (sum) hhi = sh_omz_sq, by(offid_nu ym_fd)
sum hhi, de
restore

* mean / histogram of coeff of variation in the % activeness by each type of nurses across ZIP codes within firm-month pairs

*take a mean of activeness measure b/c this excludes # visits provided to each patient and varies at the episode level
collapse (mean) fr_nv_excl_act* fnv_st1, by(offid_nu ym_fd patzip)

tempfile cs_var
save `cs_var'

bys offid_nu ym_fd: gen nzipcode = _N
sum nzipcode, de

*compute the mean & SD, 25th pctile, 75th pctile of activeness across ZIP codes at the office-month level
forval x = 1/1 {
  preserve
  collapse (mean) m`x' = fr_nv_excl_act`x' (sd) sd`x' = fr_nv_excl_act`x' (p25) p25`x' = fr_nv_excl_act`x' (p75) p75`x' = fr_nv_excl_act`x', by(offid_nu ym_fd)
  gen cov`x' = sd/m
  gen iqr`x' = p75 - p25
  tempfile cov`x'
  save `cov`x''
  restore
}

use `cov1', clear
forval x = 2/6 {
  merge 1:1 offid_nu ym_fd using `cov`x'', nogen
}

preserve
keep offid_nu ym_fd cov? iqr?
reshape long cov iqr, i(offid_nu ym_fd) j(st)

compress
outsheet using var_activeness.csv, replace comma names


*In the same firm-month, patients living in ZIP codes with higher activeness by FT nurses have higher % FT nurse visits?
use `cs_var', clear

forval x = 1/1 {
  *get median of FT nurses' activeness in the same firm-month
  preserve
  bys offid_nu ym_fd: egen med = median(fr_nv_excl_act`x')
  gen abovemed = fr_nv_excl_act`x' >= med
  *make sure each firm-month pair has both above and below median
  bys offid_nu ym_fd: egen mm = mean(abovemed)
  count if mm==0 | mm==1
  *143 out of 6805 obs
  drop if mm==0 | mm==1

  keep offid_nu ym_fd patzip abovemed fnv_st1
  collapse (mean) fnv_st1 , by(offid_nu ym_fd abovemed)

  di "---------- Status `x' -------"
  ttest fnv_st1, by(abovemed) unequal

  loc mean_above`x' : display %9.2f `r(mu_2)'
  loc mean_below`x' : display %9.2f `r(mu_1)'
  loc mean_diff`x' : display %9.2f `r(mu_2)' - `r(mu_1)'
  loc p`x' : display %9.2f `r(p)'

  restore
}

*---------------
*B. Across Time Within Firm-ZIP Code Pairs
use `cs_var', clear

*how many months per office-ZIP code pair?
preserve
keep offid_nu ym_fd patzip
duplicates drop
bys offid_nu patzip: gen nmonths = _N
keep offid_nu patzip nmonths
duplicates drop
sum nmonths, de
restore


*compute the mean & SD, 25th pctile, 75th pctile of activeness across months at the office-ZIP code level
forval x = 1/6 {
  preserve
  collapse (mean) m`x' = fr_nv_excl_act`x' (sd) sd`x' = fr_nv_excl_act`x' (p25) p25`x' = fr_nv_excl_act`x' (p75) p75`x' = fr_nv_excl_act`x', by(offid_nu patzip)
  gen cov`x' = sd/m
  gen iqr`x' = p75 - p25
  tempfile cov`x'
  save `cov`x''
  restore
}

use `cov1', clear
forval x = 2/6 {
  merge 1:1 offid_nu patzip using `cov`x'', nogen
}

preserve
keep offid_nu patzip cov? iqr?
reshape long cov iqr, i(offid_nu patzip) j(st)

compress
outsheet using var_activeness_ts.csv, replace comma names

egen ss = rowtotal(fr_nv_excl_act*)
sum ss
tab ss
drop ss

*In the same firm-ZIP code, patients starting HHC in months with higher activeness by FT nurses have higher % FT nurse visits?
use `cs_var', clear

forval x = 1/1 {
  *get median of FT nurses' activeness in the same firm-month
  preserve
  bys offid_nu patzip: egen med = median(fr_nv_excl_act`x')
  gen abovemed = fr_nv_excl_act`x' >= med
  *make sure each firm-month pair has both above and below median
  bys offid_nu patzip: egen mm = mean(abovemed)
  count if mm==0 | mm==1
  *143 out of 6805 obs
  drop if mm==0 | mm==1

  collapse (mean) fnv_st1 , by(offid_nu patzip abovemed)

  di "---------- Status `x' -------"
  ttest fnv_st1, by(abovemed) unequal

  loc mean_above`x'_ts : display %9.2f `r(mu_2)'
  loc mean_below`x'_ts : display %9.2f `r(mu_1)'
  loc mean_diff`x'_ts : display %9.2f `r(mu_2)' - `r(mu_1)'
  loc p`x'_ts : display %9.2f `r(p)'

  restore
}

*---------
* t test of % FT visits by # preoccupied closest FT nurses
use anivlmis_hosp.dta, clear

keep epiid offid_nu patzip ym_fd nclosest1_abs1 fnv_st1
duplicates drop
duplicates tag, gen(dd)
assert dd==0
drop dd

collapse (mean) nclosest1_abs1 fnv_st1, by(offid_nu ym_fd patzip)

tempfile cs_var2
save `cs_var2'


*cross-sectional variation
*get median of # closest FT nurses in the same firm-month
use `cs_var2', clear
preserve
bys offid_nu ym_fd: egen med = median(nclosest1_abs1)
gen abovemed = nclosest1_abs1 >= med
*make sure each firm-month pair has both above and below median
bys offid_nu ym_fd: egen mm = mean(abovemed)
count if mm==0 | mm==1
*143 out of 6805 obs
drop if mm==0 | mm==1

keep offid_nu ym_fd patzip abovemed fnv_st1
collapse (mean) fnv_st1 , by(offid_nu ym_fd abovemed)

ttest fnv_st1, by(abovemed) unequal

loc x = 1
loc mean_above`x'_n : display %9.2f `r(mu_2)'
loc mean_below`x'_n : display %9.2f `r(mu_1)'
loc mean_diff`x'_n : display %9.2f `r(mu_2)' - `r(mu_1)'
loc p`x'_n : display %9.2f `r(p)'

restore


*time-series variation
*get median of FT nurses' activeness in the same firm-month
preserve
bys offid_nu patzip: egen med = median(nclosest1_abs1)
gen abovemed = nclosest1_abs1 >= med
*make sure each firm-month pair has both above and below median
bys offid_nu patzip: egen mm = mean(abovemed)
count if mm==0 | mm==1
*143 out of 6805 obs
drop if mm==0 | mm==1

collapse (mean) fnv_st1 , by(offid_nu patzip abovemed)

ttest fnv_st1, by(abovemed) unequal

loc mean_above`x'_ts_n : display %9.2f `r(mu_2)'
loc mean_below`x'_ts_n : display %9.2f `r(mu_1)'
loc mean_diff`x'_ts_n : display %9.2f `r(mu_2)' - `r(mu_1)'
loc p`x'_ts_n : display %9.2f `r(p)'

restore

*export to a table
use `cs_var2', clear
keep in 1/2
gen Instruments = ""
keep Instruments
replace Instruments = "Full-time nurses' activeness" if _n==1
replace Instruments = "\shortstack{Number of preoccupied\\nearest full-time nurses}" if _n==2
gen above_cs = .
gen below_cs = .
gen diff_cs = .
gen p_cs = .
gen above_ts = .
gen below_ts = .
gen diff_ts = .
gen p_ts = .

loc x 1
foreach l in "above" "below" "diff" {
  di "`l'"
  replace `l'_cs = `mean_`l'`x'' if _n==1
  replace `l'_cs = `mean_`l'`x'_n' if _n==2

  replace `l'_ts = `mean_`l'`x'_ts' if _n==1
  replace `l'_ts = `mean_`l'`x'_ts_n' if _n==2
}
loc l "p"
replace `l'_cs = `p`x'' if _n==1
replace `l'_cs = `p`x'_n' if _n==2
replace `l'_ts = `p`x'_ts' if _n==1
replace `l'_ts = `p`x'_ts_n' if _n==2

foreach v of varlist above_cs above_ts below_cs below_ts diff_cs diff_ts {
  gen `v'2 = subinstr(string(`v', "%9.2g"), ".", "0.",.)
  drop `v'
}
replace diff_ts2 = "0.20" if diff_ts2=="0.2"
replace above_ts2 = "0.60" if above_ts2=="0.6"

replace diff_cs2 = diff_cs2 + "***" if p_cs <= 0.01
replace diff_cs2 = diff_cs2 + "**" if p_cs <= 0.05 & p_cs > 0.01
replace diff_cs2 = diff_cs2 + "*" if p_cs <= 0.1 & p_cs > 0.05
replace diff_ts2 = diff_ts2 + "***" if p_ts <= 0.01
replace diff_ts2 = diff_ts2 + "**" if p_ts <= 0.05 & p_ts > 0.05
replace diff_cs2 = diff_cs2 + "*" if p_ts <= 0.1 & p_ts > 0.05

foreach v in "above_cs" "above_ts" "below_cs" "below_ts" "diff_cs" "diff_ts" {
  rename `v'2 `v'
}

order Instruments above_cs below_cs diff_cs above_ts below_ts diff_ts
outsheet using `reg'/ttest.csv, names comma replace

*----------------------------------------
* cross-sectional, time-series variation in the number of preoccupied nearest full-time nurses



*----------------------------------------------------------
* mean distance to actual full-time nurses who visited her
*----------------------------------------------------------
use anivlmis_hosp, clear
keep epiid patzip
duplicates drop

*merge with visit level data to get a list of actual nurses who visited each patient
merge 1:m epiid using visit_worker_chars, keep(1 3) nogen
keep if discipline=="SN"

*create month of the worker's visit date
capture drop yr
gen yr = year(visitdate)
gen mo = month(visitdate)
gen ym_visitd = ym(yr, mo)
format ym_visitd %tm

*get worker's ZIP code
merge m:1 payrollno workerID using staff_chars, keepusing(zipcode) keep(1 3) nogen
rename zipcode workerzip

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

*get a list of actual nurses who served the patient episode from the visit data
*get visit-level data
use `tmp', clear
/* keep epiid offid_nu ym_visitd payrollno patzip workerzip st
duplicates drop

merge m:1 workerzip offid_nu payrollno st ym_visitd using `active_nurse'
*no _merge=1 - good!
*_merge = 2 means nurses who didn't visit the patient
*_merge = 3 means nurses actually visited the patient
assert _merge!=1
gen actual_nurse = _merge==3
drop _merge */

*restrict to the start of care month for the patient
bys epiid: egen ym_fd = min(ym_visitd)
keep if ym_visitd==ym_fd

keep epiid offid_nu payrollno
duplicates drop

tempfile pat_actualnurse
save `pat_actualnurse'

use ompw_new, clear

*restrict to episodes in the IV analysis
merge m:1 epiid using anivlmis_hosp, keepusing(epiid) keep(3) nogen

*in this list of patient's all active nurses in each office-month, match with the patient's actual nurses to exclude them for each patient, by patient episode-month-office-payrollno
merge m:1 epiid offid_nu payrollno using `pat_actualnurse', keep(1 3)
*no _merge=1 - good!
*_merge = 2 means nurses who didn't visit the patient
*_merge = 3 means nurses actually visited the patient

bys epiid: egen mm = max(_merge)
assert mm==3
drop mm

*drop _merge==3 obs who are the nurses actually visited those patients
gen actual_nurse = _merge==3
drop _merge

*merge with distance xwalk data
merge m:1 workerzip patzip using zipdistance, keep(1 3) nogen
drop _merge
sort epiid actual_nurse payrollno

*within patient, the difference in mean distances to the nurses by whether they actually visited the patient
preserve
collapse (mean) mdist = mi_to_zcta5, by(epiid actual_nurse)
bys epiid: gen nn= _N
assert nn==2
drop nn

sort epiid actual
reshape wide mdist, i(epiid) j(actual_nurse)
sum mdist*
ttest mdist0==mdist1 if mdist1!=.
restore

* by status within patient
preserve
collapse (mean) mdist = mi_to_zcta5, by(epiid st actual_nurse)
bys epiid st: gen nn= _N
tab nn
keep if nn==2
drop nn

sort epiid actual
reshape wide mdist, i(epiid st) j(actual_nurse)
sum mdist*
ttest mdist0==mdist1 if mdist1!=. & mdist0!=. & st==1
ttest mdist0==mdist1 if mdist1!=. & mdist0!=. & st==4

*merge with episode level distance to the closest FT nurses
merge m:1 epiid using anivlmis_hosp, keep(3) keepusing(min1dist_abs1 min2dist_abs1 min3dist_abs1) nogen

ttest min1dist_abs1==mdist1 if mdist1!=. & min1dist_abs1!=. & st==1
restore

*across patients and nurses, difference in distances to the nurses by whether they actually visited the patient
bys actual_nurse: sum mi_to_zcta5
ttest mi_to_zcta5, by(actual_nurse) unequal


*----------------------------------------
* Exogeneity of IV: Balancing of covariates
use anivlmis_hosp.dta, clear

*group into above/equal to median and below median for each instrument
loc v1 fr_nv_excl_act1
sum `v1', de
loc th1 `r(p50)'

gen gp = .
replace gp = 1 if `v1' < `th1'
replace gp = 2 if `v1' >= `th1' 


/* loc v2 nclosest1_abs1
sum `v2', de
loc th2 `r(p50)'

gen gp = .
replace gp = 1 if `v1' < `th1' & `v2' >= `th2'
replace gp = 2 if `v1' < `th1' & `v2' < `th2'
replace gp = 3 if `v1' >= `th1' & `v2' >= `th2'
replace gp = 4 if `v1' >= `th1' & `v2' < `th2' */

assert gp!=.
tab gp

bys gp: sum fnv_st1

loc officechars allepi_od tnw_SN_od fnw_stSN`ss'_od
loc ss 1
keep epiid gp fnv_st`ss' hashosp nhandoff nv_d_SN avgvgap `riskhosp' age female white ma_visit ma_epi dual noassist livealone `comorbid' `officechars'
duplicates drop
order epiid gp fnv_st`ss' hashosp nhandoff nv_d_SN avgvgap `riskhosp' age female white ma_visit ma_epi dual noassist livealone `comorbid' `officechars'

bys gp: outreg2 using `reg'/epi_summ_balance.xls, tex replace sum(log) label eqkeep(N mean) dec(2) fmt(fc)



*----------------------------------------

*merge with episode-day level data
tempfile an2
save `an2'

use `an2', clear
merge 1:m epiid using handoff_pd, keep(3) nogen
*one _m==1 obs drop it

*keep if # episodes is 1
capture drop daysinHH
bys `mvar': egen ffvd = min(visitd)
bys `mvar': egen llvd = max(visitd)
egen latest = rowmax(llvd dcdate2)
gen daysinHH = latest - ffvd + 1
gen nepi2 = ceil(daysinHH/60)
tab nepi2
drop if nepi2 > 1
drop ffvd llvd latest nepi2

*create HH day index
sort epiid visitd
capture drop hhday
gen hhday = visitdate - fd + 1
tab hhday
assert hhday >=1 & hhday <=60

*define contingent workers vs permanent ones
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st==. if status==""
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

tab hadSNvisit if lmix==1 & hhday==1
tab st if lmix==1 & hhday==1 & hadSNvisit==1

*tag episode
sort epiid visitdate
bys epiid: gen i = _n==1

*create indicator for whether the first nurse was not FTN
gen notFTNfirst = st!=1 & lmix==1 & hhday==1 & hadSNvisit==1
list epiid visitdate payrollno status vseq hadSNvisit in 1/10



*------------------------------------------------
* IV regression

*number of endogenous variables : 3
* % workers in each arrangement, total # SN visits, # handoffs during an episode
loc ss 1

loc end fnv_st`ss'
*rate_handoff nv_d_SN
loc nend 1
loc ev1 fnv_st`ss'
loc ev2 rate_handoff
loc ev3 nv_d_SN

*IV set
*loc iv1 fr_nv_excl_act*
loc iv2 fr_nv_excl_act* nclosest1_abs1
/* loc iv1 min1dist_abs1 min2dist_abs1 min3dist_abs1 min1dist_abs4 min2dist_abs4 min3dist_abs4 nclosest1_abs1 nclosest2_abs1 nclosest3_abs1 fr_nv_excl_act*
loc iv2 min1dist_rel1 min2dist_rel1 min3dist_rel1 min1dist_rel4 min2dist_rel4 min3dist_rel4 nclosest1_rel1 nclosest2_rel1 nclosest3_rel1
loc iv3 min1dist_abs1 min2dist_abs1 min3dist_abs1 min1dist_abs4 min2dist_abs4 min3dist_abs4 fr_nv_excl_act1 fr_nv_excl_act4
loc iv4 `iv1' fr_nv_excl_act1 fr_nv_excl_act4
loc iv5 min1dist_rel1 min2dist_rel1 min3dist_rel1 min1dist_rel4 min2dist_rel4 min3dist_rel4 fr_nv_excl_act1 fr_nv_excl_act4
loc iv6 `iv2' fr_nv_excl_act* */

*4 spec's for indep vars (+ office FE)
loc officechars allepi_od tnw_SN_od fnw_stSN`ss'_od
loc partial1 i.ym_fd i.dow i.wkofyr i.yr i.offid_nu i.patzip
loc partial2 `partial1' `riskhosp'
loc partial3 `partial2' `demog'
loc partial4 `partial3' `comorbid'

*IV regression
forval nn = 2/2 {
  foreach ev of varlist `end' {
    loc fsr_`ev'
  }
  *initiate the tuple of second-stage reg results
  loc ssr

  forval n=1/4 {
    loc sp1 nhandoff nv_d_SN avgvgap i.ym_fd i.dow i.wkofyr i.yr `officechars' i.patzip i.offid_nu
    loc sp2 `sp1' `riskhosp'
    loc sp3 `sp2' `demog'
    loc sp4 `sp3' `comorbid'

    eststo iv_n`n'_`nn': ivreg2 hashosp `sp`n'' (`end' = `iv`nn'') , cluster(offid_nu patzip) first savefirst savefprefix(f_n`n'_`nn'_) gmm2s partial(`partial`n'')
    capture drop insmpl
    gen insmpl = e(sample)
    *reg fnv_st`een' `sp`n'' `inst_s`nn'', vce(cluster offid_nu)

    estimates dir
    estimates save iv`n'_i`nn', replace

    mat fstat_n`n'_`nn' = e(first)

    forval j = 1/`nend' {
      estadd scalar fs_`ev`j''_`nn' = fstat_n`n'_`nn'[4,`j'] : f_n`n'_`nn'_`ev`j''
    }

    foreach ev of varlist `end' {
      loc fsr_`ev' `fsr_`ev'' f_n`n'_`nn'_`ev'
    }
    loc ssr `ssr' iv_n`n'_`nn'
    di "`ssr'"

    *get R-squared from first stage OLS regression
    foreach ev of varlist `end' {
      loc officechars allepi_od tnw_SN_od fnw_stSN`ss'_od
      loc sp1 nhandoff nv_d_SN avgvgap i.ym_fd i.dow i.wkofyr i.yr `officechars'
      loc sp2 `sp1' `riskhosp'
      loc sp3 `sp2' `demog'
      loc sp4 `sp3' `comorbid'

      reghdfe `ev' `iv`nn'' `sp`n'', vce(cluster patzip offid_nu) absorb(offid_nu patzip)
      estadd scalar fr2_`ev'_`nn' = `e(r2)' : f_n`n'_`nn'_`ev'
    }
  }

  *for each first stage, save in a separate file
  foreach ev of varlist `end' {
    loc file iv1s_`ev'_iv`nn'_st`ss'
    esttab `fsr_`ev'' using `reg'/`file'.tex, booktabs replace stats(N fr2_`ev'_`nn' fs_`ev'_`nn', fmt(0 2 2) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(3)) se(par fmt(3))) keep(`iv`nn'') order(`iv`nn'') label starlevels( * 0.10 ** 0.05 *** 0.010)

    esttab `fsr_`ev'' using `reg'/`file'.csv, replace stats(N fr2_`ev'_`nn' fs_`ev'_`nn', fmt(0 2 2) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(3)) se(par fmt(3))) keep(`iv`nn'') order(`iv`nn'') label starlevels( * 0.10 ** 0.05 *** 0.010)
  }

  *save 2nd stage reg
  loc file iv2s_iv`nn'_st`ss'
  esttab `ssr' using `reg'/`file'.tex, booktabs replace stats(r2 jp N, fmt(2 2 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(3)) se(par fmt(3))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)

  esttab `ssr' using `reg'/`file'.csv, replace stats(r2 jp N, fmt(2 2 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(3)) se(par fmt(3))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)
}

*OLS regression
loc officechars allepi_od tnw_SN_od fnw_stSN`ss'_od
loc sp1 nhandoff nv_d_SN avgvgap i.ym_fd i.dow i.wkofyr i.yr `officechars'
loc sp2 `sp1' `riskhosp'
loc sp3 `sp2' `demog'
loc sp4 `sp3' `comorbid'

loc file st`ss'_ols
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

keep if insmpl==1

* reference for reghdfe package: http://scorreia.com/software/reghdfe/quickstart.html

forval n=1/4 {
   reghdfe hashosp `end' `sp`n''  , vce(cluster offid_nu patzip) absorb(offid_nu patzip)

   sum hashosp if e(sample)
   loc mdv: display %9.2f `r(mean)'

   loc rsq: display %9.2f e(r2)
   loc ar2: display %9.2f `e(r2_a)'
   `out' keep(`end' ) label addtext(R-squared, `rsq', Adjusted R-squared, `ar2', Mean dep. var., `mdv')
}

*------------------------------
*estimate the richest specification separately for different disease groups

use anivlmis_hosp, clear
*use Charlson comorbidity groups with enough patients; do not mutually exclude people across groups
sum ynch*

gen dx = .
loc x = 0
foreach v of varlist ynch* {
  loc x = `x' + 1
  di `x'
  replace dx = `x' if `v'==1
}
tab dx
*6 - Chronic pulmonary disease (COPD), 2 - Congestive heart failure (CHF), 14 - Cancer
gen dxd = "COPD"
sum ynch6 ynch2 ynch14 if ynch2==1

foreach v of varlist ynch* {
  egen m_`v' = mean(`v')
}

/* *first, get the DX categories
use comorbidity, clear
keep admissionclie burns-ulcer
duplicates drop

gen dx = .
gen dxd = ""
loc x = 0
foreach v of varlist burns-ulcer {
  loc x = `x' + 1
  di `x'
  replace dx = `x' if `v'==1
  replace dxd = "`v'" if `v'==1
}
tempfile comorbidity
save `comorbidity' */

use anivlmis_hosp, clear
drop _merge
merge 1:1 admissionclie using `comorbidity', keep(1 3) nogen
tab dx, sort
* top 5:
bys dx: gen top = _N
keep if top > 1000


*------------------------------
*estimate the richest specification separately for different firm age
