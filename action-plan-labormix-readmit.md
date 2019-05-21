# Action plan for the paper “Alternative Work Arrangement and Performance: Evidence from Nurses in Home Health Care”

##	Story
If a patient sees a nurse in alternative work arrangement, does it increase the risk of rehospitalization?

##	Main analysis

##	Mechanism
1. Did the temp nurses see sicker/tougher cases? Or full-time nurses hand over patients when patients get sicker?
  - Then it’s not about the alternative work arrangement, but the patient was already sicker
2.	Is it unfamiliarity with the patient? Did the temp nurses get assigned to patients they’ve never seen?
  -	Check whether temp nurse is more frequently assigned to a patient they’ve never seen than full-time nurses
  -	Compare the effect when the temp nurse is assigned to a patient they’ve seen vs a patient they’ve never seen
3.	Is it the unobserved quality/competence disparity between standard and alternative work arrangements?
  -	It’s not about skills if a same worker who switches the work arrangements see a patient, and has a different patient outcome: Re-run using the subsample of nurses that switched status and patients served when the switch occurred
  -	Also, how many temp workers switch to full-time position? If temp position is used as a stepping stone, then temp workers are not necessarily worse than full-time nurses
4.	Is it work experience or organizational attachment?
  -	Stratify by the work experience of workers
  -	Potentially, I can look up each worker’s name (`staff\Workers by Office_Workers by Office.csv`) in the RN directory on each state website (e.g. [Connecticut website](https://www.elicense.ct.gov/lookup/licenselookup.aspx)) and check the license granted date
5.	Is it different educational attainment?
  -	Stratify by the educational degree, if available
6.	Is it full-time nurses getting paid more (efficiency wage hypothesis)?
7. Is it salary vs piece-rate pay that's driving the difference?

##	Sub-analysis: Rebut any skepticism or provide more detailed insight
1.	Is the main effect concentrated among a few patients?
2.	Maybe the effect reflects the negative effect of handoff
  -	Show that the effect of handoff from full-time to temp worker is greater than the effect of handoff from temp to full-time worker
3.	Does seeing a temp nurse matter when patients have different length of HHC?
  -	Stratify by the duration of HHC
4.	Does alternative work arrangement matter for LPNs and HH aides?

## headache problems
-	How should I think of the effect of handoffs along with temp workers
  -	Run separately with and without handoff controls
  -	Scatterplot of handoffs vs % visits by alternative work arrangement
