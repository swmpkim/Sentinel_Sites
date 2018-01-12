## SET data workup


### Guide to scripts

+ `QC_plots.R`: a first attempt at some general graphs that might help with quality checks.  
+ `Rates.Rmd`: Calculations of change rates for SETs and Marker Horizons.  
+ `Analyses.Rmd`: ANOVAs and Tukey Tests to compare change rates between sites. t-tests to compare elevation change rates within sites to local rate of sea level rise.  
+ `SETS_parent_file.Rmd`: File that runs both `Rates` and `Analyses` scripts and provides all documentation, folded code, and outputs into one html file.  



*****


### Guide to spreadsheets  

+ `markers_UPDATED.csv`: Marker horizon data; current and correct as of 2017-12-13.  
  +  Dates have been verified from field notes.  
  +  Mix-ups have been corrected: SPAL-1 and SPAL-3 were swapped in the note-taking on 2014-08-05 and have been corrected here; PANNE-1 and PANNE-3 were swapped on 2015-06-03 and are corrected here.  
  +  A column called *ref_date* has been added; this represents the most recent (on or before reading date) laying of feldspar.  
  +  Column names: SET (e.g. CLMAJ-1, CLMAJ-2, SPAL-1, etc.); MHplot(1, 2, or 3); date; ref_date; A, B, C, D - all for readings on a core within a plot on a given date.
+ `sets_wide_UPDATED.csv`: Pin readings; current and correct as of 2017-12-13.  
  +  Dates have been verified from field notes.  
  +  Mix-ups have been corrected: SPAL-1 and SPAL-3 were swapped in the note-taking on 2014-08-05 and have been corrected here; PANNE-1 and PANNE-3 were swapped on 2015-06-03 and are corrected here.  
  +  Column names: SET (e.g. CLMAJ-1, CLMAJ-2, SPAL-1, etc.); date; arm (1L, 3R, 5R, or 7L); pin_1, pin_2, ..., pin_9 (one row per arm per date; one column per pin in that arm position).
+ `NAVD88.csv`: NAVD-88 elevations for SET heads and adapters.  
  +  the date these readings were taken is not known; might be in the `NAVD88.xlsx` spreadsheet.  
  +  Column names: Site (e.g. CLMAJ-1, CLMAJ-2, SPAL-1, etc.); SET_NAVD88; Adapter_NAVD88
+ `SETdataspreadsheet_With_NERR_Stations_Baseline_MASTER.xlsx` is the master spreadsheet that all these readings came from.  
  +  Dates are not correct!!! Not all SETs or Marker Horizons were read on the same date.
  +  The PANNE-1/3 swap on 2015-06-03 was corrected in this sheet, but MH readings that date were NOT.
  +  The SPAL-1/3 swap on 2014-08-05 has NOT been corrected in this sheet - neither pins nor MHs.
  +  There are a lot of formulas in this spreadsheet that seem to have had errors introduced over the years with column insertions and copying/pasting. That's why I've split it out into smaller csv files, where dates and data could be corrected; any calculations will be done in R.



