# Alternative Work Arrangement and Performance: Evidence from Nurses in Home Health Care
This project examines the effect of alternative work arrangement on performance using data on home health nurses, the most crucial determinant of quality of care in home health.  

1. `crsingle_epi_visitlvl.do`
  - create the base sample of admission-episode-visit level data
2. `crsingle_epi_daylvl.do`
  - create a episode-day level dataset from the episode-visit level data
3. `crsingle_epi_daylvl_an.do`
  - fine-tune the sample for regression analysis by applying sample restriction rules & filling in missing values in patient characteristics
