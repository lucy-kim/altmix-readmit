*run regression of readmission indicator on the indicators of seeing different types of nurses using the patient-day level sample
*refer to anhandoff_pd_pub.do

set matsize 10000
loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

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

cd `path'

*------------------------
*OLS LPM
use single_epi_daylvl_an, clear
sort epiid visitdate
list epiid visitdate st ho hadSNvisit in 1/60

*model specification
loc ages i.agecat
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc demog i.agecat female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*depressed
loc scale ln_allepi ln_nw_active_SN

loc sp1 `scale' i.dow i.ym i.hhday
loc sp2 `sp1' `riskhosp'
loc sp3 `sp2' `demog'
loc sp4 `sp3' `comorbid'
*sum `sp4'

loc xvar i.st ho hadSNvisit
loc y hospoccur

loc file pd_ols
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

forval n=1/4 {
  areg `y' `xvar' `sp`n'' , vce(cluster offid_nu) absorb(offid_nu)

  *mean dep var
  sum `y' if e(sample)
  loc mdv: display %9.4f `r(mean)'

  loc addtxt "Hosp risk controls, `hosprisk`n'', Demographic controls, `demog`n'', Comorbidity controls, `comb`n''"
  di "`addtxt'"

  `out' keep(`xvar') addtext(Mean dep. var., `mdv',`addtxt') dec(4) fmt(fc)
}

*composition of visits across work arrangements
loc n 4
areg `y' `xvar' `sp`n'' , vce(cluster offid_nu) absorb(offid_nu)
tab st if e(sample)
                   /* st |      Freq.     Percent        Cum.
----------------------+-----------------------------------
   Full-time salaried |    599,891       59.50       59.50
 Part-time w/ benefit |     96,504        9.57       69.07
Part-time w/o benefit |     46,352        4.60       73.67
              On-call |    183,919       18.24       91.91
           Contractor |        944        0.09       92.00
                Other |     80,645        8.00      100.00
----------------------+-----------------------------------
                Total |  1,008,255      100.00 */

*exclude patients visited only by non-full-time nurses
use single_epi_daylvl_an, clear
bys epiid: egen a = min(st)
sum a
drop if a > 1

loc file pd_ols_noonlytemp
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

forval n=1/4 {
  areg `y' `xvar' `sp`n'' , vce(cluster offid_nu) absorb(offid_nu)

  *mean dep var
  sum `y' if e(sample)
  loc mdv: display %9.4f `r(mean)'

  loc addtxt "Hosp risk controls, `hosprisk`n'', Demographic controls, `demog`n'', Comorbidity controls, `comb`n''"
  di "`addtxt'"

  `out' keep(`xvar') addtext(Mean dep. var., `mdv',`addtxt') dec(4) fmt(fc)
}

table st if e(sample), contents(mean hadSNvisit mean ho mean hospoccur)
----------------------------------------------------------------------
                   st | mean(hadSNv~t)        mean(ho)  mean(hospoc~r)
----------------------+-----------------------------------------------
   Full-time salaried |       .2006264        .2549163        .0051976
 Part-time w/ benefit |       .2187555        .3964833        .0044923
Part-time w/o benefit |       .2170211        .4612207        .0056601
              On-call |       .2130574        .5641121        .0045617
           Contractor |       .1599045        .7708831        .0071599
                Other |       .2216902         .502332        .0046017
----------------------------------------------------------------------
