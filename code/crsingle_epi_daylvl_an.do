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

compress
save single_epi_daylvl_an, replace

*may drop AIDS, blood loss anemia, cancer later
