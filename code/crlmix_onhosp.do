*what is the effect of labor mix on probability of (re)hospitalization?
*analyze at the episode level, episode-day level

loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'
loc mvar admissionclie
loc nepi_byo_thresh 30
loc nv_SN_thresh 20
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
set matsize 11000

*controlling vars
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc demog age female white `ins' noassist livealone
loc comorbid ynch* `overallst'  `hrfactor' `priorcond'
loc officechars allepi_od tnw_SN_od fnw_prSN_od
*depressed
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

*---------------------------
*office sample restriction to firms aged at most 6 years
*---------------------------
/* use anpanel_demandfl, clear
keep if firmage_yrbin <= 5 & exist_noff_county==0
keep if openaft2012==1 | (openaft2012==0 & open_noff_county==0)
keep offid_nu
duplicates drop
tempfile office
save `office' */

*------------------------------------
* episode-level analysis: fraction of full-time worker provided visits with the indicator for having a rehospitalization
*------------------------------------
use rehosp_patsmpl_visit, clear

keep if nepi==1

*restrict to offices aged <= 7
/* merge m:1 offid_nu using `office', keep(3) nogen */

*drop if the patient had no SN visits
gen snv = discipline=="SN"
bys epiid: egen tsnv = sum(snv)
drop if tsnv==0

tempfile epibase
save `epibase'


/* *create a patient-day level data to find the cumulative visit distribution during the home health care
use `epibase', clear
bys epiid: egen fd = min(visitdate)
bys epiid: egen ld = max(visitdate)
gen dd = ld - fd + 1
keep dd epiid fd ld
duplicates drop
expand dd

sort epiid fd
bys epiid: gen visitdate_e = fd + _n - 1
format visitd %d

*create HH day index
assert dd <= 60
sort epiid visitdate
bys epiid : gen hhday = visitdate - fd + 1
assert hhday <= 60

*merge with episode level data to get visit indicator for each patient-day
merge 1:m epiid visitdate_e using `epibase', keepusing(offid_nu discipline)
gen snv = discipline=="SN" & _merge==3

*SN visit statistic by HH day

*count the total # patients who are receiving home health on each HH day
sort hhday epiid visitdate
bys hhday epiid: gen i = _n==1
bys hhday: egen totpat = sum(i)

*count the total # patients receiving SN visit
gsort hhday epiid -snv
capture drop i
bys hhday epiid: gen i = _n==1 & snv==1
bys hhday: egen totsnv = sum(i)

*probability of receiving SN visit on each HH day
gen probsnv = totsnv/totpat

tab hhday, sum(probsnv)

*cumulative percent of visits within an episode
sort epiid visitdate
bys epiid: gen cumsnv_epi = sum(snv)
bys epiid: egen tsnv_epi = sum(snv)
gen cumsnv = cumsnv_epi / tsnv_epi
list epiid visitdate snv cumsnv_epi tsnv cumsnv in 1/10

*mean cumulative percent
tab hhday, sum(cumsnv)
binscatter cumsnv hhday, discrete
graph export `gph'/cumsnv.eps, replace

*export to csv file for visualization in R
preserve
keep cumsnv hhday
outsheet using cumsnv.csv, replace comma
restore
sum tsnv_epi */

*------------------------------------------------
* what is the fraction of visits provided by contingent workers for each discipline?
use `epibase', clear

des discipline status
assert discipline!=""
drop if status==""
*1 obs

*drop if age < 65
tab age
count if age < 65 | age > 100
drop if age < 65 | age > 100

*drop cognitively/mentally vegetative/nonresponsive patients
count if mentalfine!=.
keep if mentalfine==1

rename black black
rename hispan hispanic

*drop offices whose # episodes are < 30
assert offid_nu!=.
assert epiid!=.
sort offid_nu epiid visitdate visittime
bys offid_nu epiid: gen j = _n==1
capture drop nepi
bys offid_nu : egen nepi = sum(j)
capture drop i
bys offid_nu: gen i = _n==1
count if i==1
*101 offices
tab addr_st if i==1
*17 states
count if i==1 & nepi < `nepi_byo_thresh'
*9 offices have total number of episodes < 30
tab addr_st if i==1 & nepi < `nepi_byo_thresh'

*drop 9 offices
tab offid_nu if nepi < `nepi_byo_thresh'
drop if nepi < `nepi_byo_thresh'
capture drop i j


*calculate total number of visits for each discipline
drop snv tsnv
sort epiid discipline visitdate visittime
capture drop i
*bys epiid discipline visitdate visittime: gen i = _n==1
gen i = 1
bys epiid discipline: egen nv_d = sum(i)
sort epiid visitdate discipline

*distinguish salaried vs piece-rate workers

*define contingent workers vs permanent ones
assert status!=""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Office/Other", replace

*fraction of visits provided by contingent workers for each discipline
bys epiid discipline st: egen nv_ds = sum(i)
sort epiid discipline st
gen fnv_st = nv_ds/nv_d
*some episode-discipline obs have visits provided by only salaried workers, then just put 0

*day of week of last day of episode
sort epiid visitdate
bys epiid: egen ld = max(visitdate)
gen dow = dow(visitdate) if visitdate==ld
bys epiid: egen dow2 = max(dow)
drop dow
rename dow2 dow
lab var dow "day of week of last day of episode"

*does the hospitalization rate differ by day of week among the last days of episode?
bys dow: sum hashosp if visitdate==ld
*on sunday (44% hosp rate), saturday (29%), monday (19%), the rest (13-16%)
*can I use day of week of the last day of episode as instrument?

*year-month variable for the episode start & last day
sort epiid visitdate
bys epiid: egen fd = min(visitdate)
foreach v of varlist fd ld {
    capture drop month year
    gen year = year(visitdate) if visitdate==`v'
    gen month = month(visitdate) if visitdate==`v'
    capture drop ym_`v'
    gen ym_`v' = ym(year,month) if visitdate==`v'
    drop year month

    bys epiid: egen ym_`v'2 = max(ym_`v')
    drop ym_`v'
    rename ym_`v'2 ym_`v'
}
lab var ym_fd "Year-month of first visit day of an episode"
lab var ym_ld "Year-month of last visit day of an episode"

*what is the day in a month the patients started HHC?
gen day = day(visitdate)
tab day if visitdate==fd

tempfile visitlevel
save `visitlevel'

*-----------------
* create number of SN handoffs during the episode for each episode
*have to create a handoff indicator
use `visitlevel', clear

keep if discipline=="SN"

*create a hand-off indicator =1 when payrollno changes to a different one on the next visit at a different time
sort epiid visitdate visittime
bys epiid: gen lastworker = payrollno[_n-1] if _n > 1
bys epiid: gen ho = payrollno!=lastworker if _n > 1

*are there any episodes who have different workers visiting the patient at the same time?
bys epiid visitdate visittime payrollno: gen l = _n==1
bys epiid visitdate visittime: egen sl = sum(l)
tab sl
tab epiid if sl==2
*116537, 216682, 403989

/* list epiid visitdate visittime payrollno ho sl if epiid==491281 */

*manually change the hand-off indicator for these 3 episodes so that if one of the workers visiting at the same time is same as the previous one, HO = 0 (except 116537)
bys epiid: replace ho = 0 if epiid==216682 & (sl==2 | sl[_n-1]==2)
*epiid 116537 should have a handoff = 1 b/c diferent worker visits
bys epiid: replace ho = . if epiid==403989 & sl==2

bys epiid: replace ho = 0 if epiid==258625 & sl==2

bys epiid: replace ho = 0 if epiid==491281 & sl==2

*if there are multiple workers visiting at the same time-day for an episode, count ho once only
sort epiid visitdate visittime
bys epiid visitdate visittime: gen c = _n==1
replace ho = . if c==0

*number of hand-offs during an episode
bys epiid: egen nhandoffs = sum(ho)
lab var nhandoffs "total # SN handoffs during the episode"

keep epiid nhandoffs
duplicates drop

tempfile nhandoffs
save `nhandoffs'

*-----------------

*collapse to episode-level data
use `visitlevel', clear

*get # visits for each discipline for each episode
capture drop j
gen j = 1
collapse (sum) nv_d_ = j , by(epiid discipline)

*first spread out by discipline
reshape wide nv_d_, i(epiid) j(discipline) string

*fill in missing values with 0
foreach v of varlist nv_d_* {
  replace `v' = 0 if `v'==.
}

tempfile nv_d
save `nv_d'

*collapse to episode-level data
use `visitlevel', clear

keep if discipline=="SN"

collapse (mean) hashosp nv_ds fnv_st `riskhosp' `demog' `comorbid' dow ym_fd ym_ld, by(epiid offid_nu st epidate2 endstatus epienddate)

*reshape wide so that each obs = episode
sort epiid st
list epiid st nv_d_SN nv_ds fnv_st in 1/10

reshape wide nv_ds fnv_st, i(epiid offid_nu epidate2 endstatus epienddate) j(st)
*`riskhosp' `demog' `comorbid' dow ym_fd ym_ld hospi

*fill in missing values in # visits by each status of worker
foreach v of varlist nv_ds* {
    replace `v' = 0 if `v'==.
    assert `v'!=.
}

*the proportion of visits by each status of worker should be 0 if the # visits by that status is 0
forval x = 1/6 {
    replace fnv_st`x' = 0 if nv_ds`x'==0
    assert fnv_st`x'!=.
}

*merge with episode-level # visits in each discipline
merge 1:1 epiid using `nv_d', nogen

*total number of visits during episode
egen tnv = rowtotal(nv_d_*)

*merge with data on # handoffs per episode
merge 1:1 epiid using `nhandoffs', keep(1 3) nogen
assert nhandoff==0 if nv_d_SN==1
*if at the same time, two workers visit together and one is PR & the other SA, it's too complicated. so just count these visits as 2 visits.

tempfile tmphosp
save `tmphosp'

preserve
*merge with data containing admissions
use epi_visit, clear
keep epiid `mvar' imprv* stabl*
duplicates drop

tempfile id
save `id'
restore

use `tmphosp', clear
merge 1:1 epiid using `id', keep(1 3) nogen

*merge with admission-level Charlson index var
merge 1:1 `mvar' using comorbidity, keep(1 3) keepusing(charlindex) nogen

*merge with patient ZIP code
merge m:1 epiid using client_chars, keepusing(patzip clientid) nogen keep(1 3)

lab var imprv_oralmed "Improvement in management of oral meds"
lab var imprv_trnsf "Improvement in transferring"
lab var imprv_ambul "Improvement in ambulation"
lab var imprv_bath "Improvement in bathing"
lab var imprv_freqpain "Improvement in frequency of pain interfering"
lab var imprv_whendys "Improvement in short of breath"
lab var stabl_oralmed "Stabilization in management of oral meds"
lab var stabl_trnsf "Stabilization in transferring"
lab var stabl_ambul "Stabilization in ambulation"
lab var stabl_bath "Stabilization in bathing"
lab var stabl_freqpain "Stabilization in frequency of pain interfering"
lab var stabl_whendys "Stabilization in short of breath"

*week of the patient's start of care
*merge with client chars data for the SOC date
preserve
use `visitlevel', clear
keep epiid ld fd
duplicates drop
tempfile latest
save `latest'
restore

merge 1:1 epiid using `latest', keep(1 3) nogen
lab var ld "last visit/DC/hosp date for the episode"
lab var fd "first visit date for the episode"

*monday of the week to which the episode end date belongs
capture drop day
gen day = dow(ld)
capture drop monday
gen monday = ld - 6 if day==0
forval d = 1/6 {
    loc d2 = `d' - 1
    replace monday = ld - `d2' if day==`d'
}
format monday %d
drop day

* keep weeks starting from 1/2/2012 - 7/27/2015
drop if monday < mdy(1,2,2012)

tempfile pre
save `pre'
/* compress
save pre, replace */

*merge with office-day level variables for labor supply & service demand
use daily_officepanel, clear

*create salaried vs piece-rate positions
/* gen salaried = status=="VFT" | status=="VPB" | status=="VPC" | status=="SFT" | status=="SPB" | status=="SPC" | status=="EXEMPT"
gen piecerate = 1 - salaried */
*define contingent workers vs permanent ones
drop if status==""
gen st = 1 if status=="VFT"
replace st = 2 if status=="VPB"
replace st = 3 if status=="VPC"
replace st = 4 if status=="VPD"
replace st = 5 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 6 if status=="EXEMPT" | status=="SFT" | status=="SPB" | status=="SPC" | status=="PTN" | status=="PTP" | status=="STATP" | status=="STATN" | status=="FT" | status=="SPD" | status=="NON EXEMPT-PT" | status=="NON-EXEMPT-HR" | status=="unknown"
assert st!=.
lab define stl 1 "Full-time salaried" 2 "Part-time w/ benefit" 3 "Part-time w/o benefit" 4 "On-call" 5 "Contractor" 6 "Other", replace
lab val st stl

keep nw_active_SN offid_nu visitdate st
collapse (sum) nw_active_SN, by(offid_nu visitdate st)
bys offid_nu visitdate: egen tnw_SN = sum(nw_active_SN)
gen fnw_stSN = nw_active_SN / tnw_SN

reshape wide fnw_stSN nw_active_SN, i(offid_nu visitdate tnw_SN) j(st)

foreach v of varlist nw_active_SN* {
    replace `v' = 0 if `v'==.
}
forval x = 1/6 {
    replace fnw_stSN`x' = 0 if fnw_stSN`x'==. & nw_active_SN`x'==0 & tnw_SN > 0
    assert fnw_stSN`x' ==. if tnw_SN==0
}

compress
tempfile officedaily
save `officedaily'

*create office-day level # SN visits, # all & new episodes
use daily_officepanel, clear
collapse (sum) nv_SN (mean) allepi newepi notnewepi, by(offid_nu visitdate)
merge 1:1 offid_nu visitdate using `officedaily', nogen

gen pch_ch_nepi = ln(allepi / notnewepi)

compress
tempfile officedaily2
save `officedaily2'

*get average of these office-day level vars across days the patient was under HHC (including non-visit days)
*create patient-day level base panel
use `pre', clear
keep epiid fd ld offid_nu
gen gap = ld - fd + 1
expand gap
sort offid_nu epiid

bys offid_nu epiid: gen visitdate_e = fd + _n - 1
format visitdate_e %d
keep offid_nu epiid visitdate

tempfile pd
save `pd'

*merge patient-day level vars with office-day level vars
use `pd', clear
merge m:1 offid_nu visitdate using `officedaily2', keep(1 3) nogen

*create mean office-level number of SN visits, % change in # episodes, # total episode, # SN workers, % of SN workforce by piece-rate workers across home health days of a patient
collapse (mean) nv_SN allepi newepi notnewepi pch_ch_nepi nw_active_SN* tnw_SN fnw_stSN*, by(epiid offid_nu)

foreach v of varlist nv_SN allepi newepi notnewepi pch_ch_nepi nw_active_SN* tnw_SN fnw_stSN* {
    rename `v' `v'_od
}

*merge back with episode level data on patient characteristics & hospitalization
merge 1:1 epiid offid_nu using `pre', nogen
capture drop _merge


compress
save lmix_onhosp_new2, replace

*----------------------
/* *Descriptive analysis:  % outcome by 3 categories--1) All PR, 2) All SA, 3) Mix for SN
use lmix_onhosp, clear
keep if fac=="Hosp"
keep if established==1

tempfile an
save `an'

/* "HHA" "SN" "MSW" "PT" */
foreach s in "SN" "HHA"  {
    use `an', clear

    *drop patients who had no SN visits
    keep if nv_d`s' > 0
    tab nv_d`s'

    *drop if # SN visits > 20 or # SN visits==1 (3642 episodes)
    drop if nv_d`s' > `nv_SN_thresh' | nv_dSN==1

    *create 3 labor mix categories
    tab fnv_pr`s'

    capture drop lmix
    gen lmix = 1 if fnv_pr`s'==0
    replace lmix = 2 if fnv_pr`s' > 0 & fnv_pr`s' <=0.5
    replace lmix = 3 if fnv_pr`s' > 0.5 & fnv_pr`s' < 1
    replace lmix = 4 if fnv_pr`s'==1
    assert lmix!=.
    lab define lmix4 1 "Only SA" 2 ">50% SA" 3 ">50% PR" 4 "Only PR"
    lab values lmix lmix4

    keep lmix fnv_pr`s' hospi imprv* stabl*
    *bys lmix: sum fnv_pr`s' hospi imprv* stabl*

    preserve
    di "----`s'-----"
    keep lmix fnv_pr`s' hospi
    *imprv* stabl*
    bysort lmix: outreg2 using "`reg'/outcdiff_bylmix_`s'.xls", replace sum(log) eqkeep(mean) tex label

    sum fnv_pr`s' hospi
    di ""
    restore
}


*----------------------
*export to R
use lmix_onhosp, clear

keep if fac=="Hosp" | fac=="Inpat Rehab" | fac=="Non-inpat" | fac=="SNF"

*restrict to sample period of Feb 2012 - July 2015
keep if ym_fd >= ym(2012,2) & ym_fd <= ym(2015,7)

*keep only the offices that have been around during the whole sample period
bys offid_nu: egen min = min(ym_fd)
bys offid_nu: egen max = max(ym_fd)

preserve
keep offid_nu min max
duplicates drop
*98 offices in the sample
count if min==ym(2012,2) & max==ym(2015,7)
*51 offices satisfy this condition
keep if min==ym(2012,2) & max==ym(2015,7)
keep offid_nu
tempfile officekeep
save `officekeep'
restore

merge m:1 offid_nu using `officekeep', keep(3) nogen
drop min max

*keep offices that have at least 30 episodes from each of the facility source
bys offid_nu facility: gen nepi = _N
bys offid_nu: egen min = min(nepi)
tab offid_nu if min < 30
*office 179
drop if min < 30
drop min nepi
*now there are 50 offices in the sample

*calculate total fraction of visits by contingent workers for all disciplines
foreach v in "HHA" "SN" "MSW" "OT" "PT" "RD" "ST" {
    gen nv_pr`v' = nv_d`v'*fnv_pr`v'
}
egen tnv_pr = rowtotal(nv_pr*)
gen fnv_pr = tnv_pr / tnv

tempfile tmp
save `tmp'

keep facility hospi fnv_prSN nv_dSN charlindex
compress
outsheet using lmix_onhosp.csv, replace comma names


*------------------ Regression analysis
*regression: effect of labor mix on the Pr(hospitalization)
use lmix_onhosp, clear
/* keep if fac=="Hosp" */
keep if established==1

*summary stats on outcomes by the mix
tab fac, sort

*drop patients who had no SN visits
loc s "SN"
keep if nv_d`s' > 0
tab nv_d`s'

*drop if # SN visits > 20 or # SN visits==1 (3642 episodes)
drop if nv_d`s' > `nv_SN_thresh' | nv_dSN==1

*create 3 labor mix categories
tab fnv_pr`s'

capture drop lmix
gen lmix = 1 if fnv_pr`s'==0
replace lmix = 2 if fnv_pr`s' > 0 & fnv_pr`s' <=0.5
replace lmix = 3 if fnv_pr`s' > 0.5 & fnv_pr`s' < 1
replace lmix = 4 if fnv_pr`s'==1
assert lmix!=.
lab define lmix4 1 "Only SA" 2 ">50% SA" 3 ">50% PR" 4 "Only PR"
lab values lmix lmix4

tempfile an
save `an'

*drop missing values in one comorbidity var to make the samples uniform
drop if priorcond_cath==.
drop if _Iage_66==.
*sum `sp4'

tab facility, sort

gen fac4 = 1 if facility=="Hosp"
replace fac4 = 2 if facility=="Inpat Rehab"
replace fac4 = 3 if facility=="SNF"
replace fac4 = 4 if facility=="Non-inpat"
lab define fac4_lab 1 "Hosp" 2 "IR" 3 "SNF" 4 "Home", replace
lab values fac4 fac4_lab

*4 spec's for indep vars (+ office FE)
loc sp1 i.dow i.monday i.zipcode_pat `officechars'
loc sp2 `sp1' `riskhosp'
loc sp3 `sp2' `demog'
loc sp4 `sp3' `comorbid'

*drop missing values if to make the sample size uniform across specifications for facility = non inpatient
drop if riskhosp_oth==.
drop if white==.

lab var nhandoffs "Number of SN handoffs"

foreach d in "SN" "PT" "HHA" "MSW" "OT" "ST" {
    lab var nv_d`d' "Number of `d' visits"
}

tempfile regdata
save `regdata'

use `regdata', clear
*cross-sectional relationship
loc xvar fnv_prSN nv_dSN nhandoffs
/* nv_dPT nv_dHHA nv_dMSW nv_dOT nv_dST */

forval l=1/1 {
    loc file lmix_onquality_ols`l'
    capture erase `reg'/`file'.xls
    capture erase `reg'/`file'.txt
    capture erase `reg'/`file'.tex
    loc out "outreg2 using `reg'/`file'.xls, tex append label"

    *mean dep var
    sum hospi if fac4==`l'
    loc mdv`l': display %9.3f `r(mean)'
    sum fnv_prSN if fac4==`l'
    loc sdidv`l': display %9.3f `r(sd)'

    forval n = 1/4 {
        areg hospi `xvar' `sp`n'' if fac4==`l', vce(cluster offid_nu) absorb(offid_nu)
        loc pch`l': display %9.2f 100*_b[fnv_prSN]*sdidv`l'/`mdv`l''

        loc addtxt "Hospitalization risk controls, `hosprisk`n'', Demographic controls, `demog`n'', Comorbidity controls, `comb`n''"
        di "`addtxt'"

        `out' keep(`xvar') addtext(Mean dep. var., `mdv`l'', Std Dev indep. var., `sdidv`l'', % change in rehosp, `pch`l'', `addtxt')
    }
}


*cross-sectional relationship including fraction of visits for other disciplines
use `regdata', clear
loc xvar fnv_prSN nv_dSN nhandoffs nv_dPT nv_dHHA nv_dMSW nv_dOT nv_dST

loc reg /home/hcmg/kunhee/Labor/regresults
forval l=1/1 {
    loc file lmix_onquality_ols`l'_v2
    capture erase `reg'/`file'.xls
    capture erase `reg'/`file'.txt
    capture erase `reg'/`file'.tex
    loc out "outreg2 using `reg'/`file'.xls, tex append label"

    *mean dep var
    sum hospi if fac4==`l'
    loc mdv`l': display %9.3f `r(mean)'

    sum fnv_prSN
    forval n = 1/1 {
        areg hospi `xvar' `sp`n'' if fac4==`l', vce(cluster offid_nu) absorb(offid_nu)
        loc pch`l': display %9.2f 100*_b[fnv_prSN]/`mdv`l''

        loc addtxt "Hospitalization risk controls, `hosprisk`n'', Demographic controls, `demog`n'', Comorbidity controls, `comb`n''"
        di "`addtxt'"

        `out' keep(`xvar') addtext(Mean dep. var., `mdv`l'', SD indep. var., `sdidv`l'', % change in rehosp, `pch`l'', `addtxt')
    }
}

* Binscatter plots
reg hospi fnv_prSN if fac4==1, vce(cluster offid_nu)
di "adjusted R2 = " e(r2_a)
*R2 = 0.0004

binscatter hospi fnv_prSN if fac4==1, xti(Proportion of SN visits by piece-rate nurses) yti(Probabiltiy of hospital readmission) xscale(r(-0.5 1)) yscale(r(0.1 0.25)) xlabel(-0.5(0.5)1) ylabel(0.1(0.05)0.25) text(0.24 0.85 "{&beta}=0.021" "    (0.009)" "Adj R{sup:2}=0.0003", just(left) linegap(2))
/* line(qfit) */
graph save `gph'/hosp_fnvprSN_noctrl, replace asis

areg hospi fnv_prSN nv_dSN `sp4' if fac4==1, vce(cluster offid_nu) absorb(offid_nu)
di "adjusted R2 = " e(r2_a)
*R2 = 0.1462

binscatter hospi fnv_prSN if fac4==1, absorb(offid_nu) controls(nhandoffs nv_dPT nv_dHHA nv_dMSW nv_dOT nv_dST
`sp4') xti(Proportion of SN visits by piece-rate nurses) yti(Probabiltiy of hospital readmission) leg(off) xscale(r(-0.5 1)) yscale(r(0.1 0.25)) xlabel(-0.5(0.5)1) ylabel(0.1(0.05)0.25) text(0.24 0.85 "{&beta}=0.029" "    (0.006)" "Adj R{sup:2}=0.0912", just(left) linegap(2))
graph save `gph'/hosp_fnvprSN, replace asis


*-----------------
*check only those who have no handoffs & % PR visits between 0 and 1 , i.e. same workers changed the status during the episode - only 168 obs , drop this ideas

*----------------------
* what are the types of arrangements for piece-rate nurses who worked on patients in the sample? get the nurses' distribution
use `regdata', clear
keep if fac4==1
keep epiid
duplicates drop
merge 1:m epiid using visit_worker_chars, keep(1 3) nogen

preserve
keep if discipline=="SN"
keep payrollno status
assert payrollno !=""
assert status!=""
duplicates drop
bys payrollno: gen n = _N

sort payrollno status
bys payrollno: gen i = _n==1
tab n if i==1

keep payrollno
duplicates drop
count
*1546 unique nurses
restore

tab addr_st

tab status
gen st = 1 if status=="VFT" | status=="VPB" | status=="VPC"
replace st = 2 if status=="VPD"
replace st = 3 if status=="NOT EMPLOYEE-CONTRACTOR"
replace st = 4 if st==.
assert st!=.
lab define st4 1 "regular" 2 "on-call direct hire" 3 "contractor" 4 "office", replace
lab values st st4
tab st



*--------------------------------


*create episode-day level data
use `epilevel', clear

*drop if age < 65
sum age
drop if age < 65

*age groups
sum age
gen age_gt95 = age >= 95 & age!=.
xi i.age
drop _Iage_95-_Iage_110

*drop cognitively/mentally vegetative/nonresponsive patients
count if mentalfine!=.
keep if mentalfine==1

rename black black
rename hispan hispanic

*drop offices whose # episodes are < 30
assert offid_nu!=.
assert epiid!=.
sort offid_nu epiid visitdate visittime
bys offid_nu epiid: gen j = _n==1
capture drop nepi
bys offid_nu : egen nepi = sum(j)
capture drop i
bys offid_nu: gen i = _n==1
count if i==1
tab addr_st if i==1
*104 offices in 17 states
count if i==1 & nepi < `nepi_byo_thresh'
*11 offices have total number of episodes < 30
tab addr_st if i==1 & nepi < `nepi_byo_thresh'

*drop 11 offices
drop if nepi < `nepi_byo_thresh'

tempfile pre
save `pre'

*merge with office-day level employment data to calculate mean staffing levels during the episode across days the patient had a SN visit
keep epiid offid_nu visitdate discipline payrollno
merge m:1 offid_nu visitdate using `officedaily', keep(1 3)

keep if discipline=="SN"
collapse (mean) tnw_SN fnw_prSN , by(epiid offid_nu)

tempfile iv
save `iv'

*merge the episode-level data containing hospitalization outcomes with the IV data
use `regdata', clear
merge 1:m epiid using `iv', keep(1 3) nogen
*all matched

*create IV data to run IV regression in a separate do file (see ivlmix_onquality.do)
compress
save ivlmix_onquality, replace

*---------------------------------
* summary stats
use ivlmix_onquality, clear
tab fac4

*drop missing values if to make the sample size uniform across specifications for facility = non inpatient
drop if riskhosp_oth==.
drop if white==.

*publication-purpose var labels
lab var age "Age"
lab var female "Female"
lab var dual "Dual eligible"

lab var hospi "Indicator for hospital readmission"
lab var fnv_prSN "Fraction of visits by contingent nurses"
lab var tnw_SN "Mean nurse staffing level in the office"
lab var fnw_prSN "Mean fraction of active contingent nurses in the office"

foreach v in "HHA" "SN" "MSW" "OT" "PT" "RD" "ST" {
    lab var nv_d`v' "Number of `v' visits"
}

preserve
keep if fac4==1

keep tnw_SN fnw_prSN hospi fnv_prSN nv_dSN nv_dPT nv_dHHA nv_dMSW nv_dOT nv_dST
order tnw_SN fnw_prSN hospi fnv_prSN nv_dSN nv_dPT nv_dHHA nv_dMSW nv_dOT nv_dST
sum
/*sum tnw_SN fnw_prSN hospi fnv_prSN nv_dSN nv_dPT nv_dHHA nv_dMSW nv_dOT nv_dST*/
outreg2 using `reg'/lmix_onhosp_summ.xls, tex replace sum(log) label eqkeep(N mean sd)
restore




*-------------------------------------------------
*worker-level effect of employment status on quality
*control for patient size , staffing size, patient mix as well as labor mix

*what is the worker-level number of visits, readmission rates
use `visitlevel', clear

preserve
*get number of visits & number of unique episodes each worker worked on
capture drop i
gen i = 1
collapse (sum) nv = i (count) nepi = epiid, by(payrollno majordisc)
tempfile nv_byworker
save `nv_byworker'
restore

*get worker-episode-level data
keep payrollno offid_nu majordisc epiid hospi piecerate
duplicates drop
duplicates tag payrollno epiid, gen(dup)
*10K obs have workers who changed status during the same episode - it's ok
drop dup

collapse (count) nepi2 = epiid (mean) hospi, by(payrollno offid_nu majordisc piecerate)

merge m:1 payrollno majordisc using `nv_byworker', nogen

compress
save workerlevel, replace

bys piecerate: sum hospi if majordisc=="SN", de

byhist hospi, by(piecerate)
graph save test, replace asis

preserve
keep if majordisc=="SN"
keep payrollno piecerate
duplicates drop
*how many workers have mulutiple statuses?
bys payrollno: egen multiple = mean(piecerate)
gen type = "onlypr" if multiple==0
replace type = "onlysa" if multiple==1
replace type = "both" if multiple >0 & multiple <1
keep payrollno type
duplicates drop
tempfile workertype
save `workertype'
restore

merge m:1 payrollno using `workertype', keep(3) nogen



*labor mix on quality becomes better? */
