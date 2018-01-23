*create a episode-day level dataset from the episode-visit level data
*reference: crhandoff_pd.do

loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'

*create a daily panel for each patient episode
use single_epi_visitlvl, clear
keep epiid visitdate offid_nu firsthospdate
bys epiid: egen min = min(visitd)
bys epiid: egen max = max(visitd)
format min %d
format max %d
egen endpoint = rowmax(firsthospdate max)
format endpoint %d

keep epiid min endpoint offid_nu
duplicates drop
gen t = endpoint - min + 1
expand t
sort epiid
bys epiid: gen visitdate_e = min + _n - 1
format visitd %d
drop t min endpoint

tempfile daily
save `daily'
*--------------------------
*create a 0/1 indicator for handoff defined as seeing a different nurse from the last visit
use single_epi_visitlvl, clear
keep if discipline=="SN"

*are there any episodes who have different workers visiting the patient at the same time?
bys epiid visitdate visittime payrollno: gen l = _n==1
bys epiid visitdate visittime: egen sl = sum(l)
tab sl
tab epiid if sl==2
*5 episodes -> drop them
bys epiid: egen x = max(sl)
drop if x==2
drop l sl x

*are there multiple visits on the same day?
bys epiid visitdate: gen l = _N
bys epiid: egen sl = max(l)
tab sl if i==1
*1224, 19 episodes (1% together) have days when 2 & 3 visits occured, respectively
*just drop these episodes because they have a higher propensity of handoffs and probably sicker, so more likely to be readmitted
drop if sl > 1
drop l sl

*create a hand-off indicator =1 when payrollno changes to a different one on the next visit at a different time
sort epiid visitdate visittime
bys epiid: gen lastworker = payrollno[_n-1] if _n > 1
bys epiid: gen ho = payrollno!=lastworker if _n > 1

list epiid visitdate visittime payrollno lastworker ho in 1/30

*--------------------------
*create a 0/1 indicator for switch in the work arrangement of nurses

*aggregate "status" to salaried, contingent-employee, contingent-contractors, and other categories
tab payrollno if status==""
* -99P

*drop episodes that have a visit by a nurse with missing work arrangement
gen miss = status==""
bys epiid: egen x = max(miss)
drop if x==1

*define each work arrangement in a detailed way
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

sort epiid visitdate visittime
bys epiid: gen lastst = st[_n-1] if _n > 1
bys epiid: gen sw_st = st!=lastst if _n > 1

list epiid visitdate payrollno ho st sw_st in 1/30

*merge with daily base panel of episode data
merge 1:1 epiid visitdate using `daily'
assert _merge!=1
*_m=2 means no visits during those days

*create
gen hadSNvisit = _merge==3
drop _merge

*drop if episodes have only non-SN visits
bys epiid: egen snv = sum(hadSNvisit)
drop if snv==0

*create a visit sequence number
sort epiid visitdate visittime
capture drop vseq
bys epiid: gen vseq = sum(hadSNvisit)

*the visit sequence # should be same if the visit time is same
bys epiid: replace vseq = vseq[_n-1] if visitdate==visitd[_n-1] & visittime==visittime[_n-1] & _n > 1
bys epiid: replace vseq = vseq[_n-1] + 1 if vseq > vseq[_n-1]+1 & _n > 1
assert vseq !=.

foreach v of varlist st ho lastst sw_st {
    bys epiid: replace `v' = `v'[_n-1] if `v'[_n-1]!=. & `v'==.
}
assert ho==. if vseq==1

foreach v of varlist payrollno lastworker {
    bys epiid: replace `v' = `v'[_n-1] if `v'[_n-1]!="" & `v'==""
}

gsort epiid -facility
foreach v of varlist facility {
    bys epiid: replace `v' = `v'[_n-1] if `v'[_n-1]!="" & `v'==""
}

*define 2 work arrangements largely: full-time vs not full-time
gen ft = st ==1
gen lastft = lastst==1

*define handoff from full-time to non-full-time & other handoffs
gen ho_ft_nft = ho==1 & lastft==1 & ft==0 if vseq > 1
gen ho_oth = ho==1 & ho_ft_nft==0 if vseq > 1
assert ho==0 if ho_ft_nft == 0 & ho_oth==0 & vseq > 1

sort epiid visitdate visittime

compress
save single_epi_daylvl, replace



/* *--------------------------
* restrict to patients with prior hospitalization
use single_epi_daylvl, clear
keep if facility=="Hosp"

*% first visits provided by FT vs non FT visits
preserve
keep if vseq==1

restore

*if the patient saw a mix of FT & non-FT nurses, how many switches from FT to non-FT nurses did the patient experience? is it usually once?
preserve
/* *drop patients who only saw FT nurses
bys epiid: egen stmax = max(st)
drop if stmax ==1 */

*drop patients who only saw non-FT nurses
bys epiid: egen stmin = min(st)
drop if stmin > 1

list epiid visitdate st lastst ho* in 1/10

*get the work arrangement of the first nurse visiting her
gen fvworkerst = st if vseq==1

*create an indicator for seeing a non-FT nurse
gen nft = 1- ft

*count handoffs only when the visit occurred
foreach v of varlist ho_ft_nft ho_oth ho nft {
    replace `v' = `v'*hadSNvisit
}

*get episode level # handoffs & # nurse visits
collapse (sum) ho_ft_nft ho_oth ho hadSNvisit nft (mean) fvworkerst charlindex, by(epiid)

foreach v of varlist nft ho ho_ft_nft ho_oth {
    gen chance_`v' = `v' / hadSNvisit
}

lab val fvworkerst stl
tab fvworkerst

tab fvworkerst, summarize(hadSNvisit)
tab fvworkerst if hadSNvisit <= 10, summarize(hadSNvisit)
tab fvworkerst if hadSNvisit <= 10, summarize(chance_nft)
tab fvworkerst if hadSNvisit <= 10, summarize(chance_ho)
tab fvworkerst if hadSNvisit <= 10, summarize(chance_ho_ft_nft)
tab fvworkerst if hadSNvisit <= 10, summarize(chance_ho_oth)

tab fvworkerst, summarize(charlindex)

sum hadSNvisit nft ho_ft_nft chance_ho_ft_nft if hadSNvisit <= 10
restore


*--------------------------
*what are all combinations of work arrangements when handoff occurs? e.g. FT -> OC, OC-> FT
use single_epi_daylvl, clear
keep st lastst ho
tab ho
keep if ho==1
gen n = 1
collapse (sum) n, by(st lastst)
lab val lastst stl
egen t = sum(n)
gen prop = 100* n/t
gsort -prop
list */
