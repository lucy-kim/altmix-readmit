*run regression of readmission indicator on the indicators of seeing different types of nurses using the patient-day level sample
*refer to anhandoff_pd_pub.do

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

use single_epi_daylvl_an, clear

*when the nurse is 
