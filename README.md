Project: Acne Analysis 
PI: Robert Dellavalle, Lisa Schilling
Create by: Grace Bosma

There are a few main documents here: 

Logo.png is used in RMD file to attach CIDA logo to output

ATLAS folder: Contains 7 files; one for each drug of interest that lists all associated drug concept ID's

P20023DellavalleSchilling_Code_acne_omop_firststep.R: Pulls data from Google Cloud Query and writes the data locally to csv files (reading directly from GBQ takes some time -- reading in from a locally saved file is much quicker)

Acne med and dosing RD x'ed medication_dosinginfo.xlsx: Used in filtering eligible drugs. If a drug is not explicately listed in this xlsx, it is assumed to have a minimum requred quantity of 27. This allows us to presume that the antibiotics were in fact prescribed for Acne and not another condition. 

Acne_Data_Management.R: Cleaning and writing clean datasets locally to "cleaned/" folder

Acne_Report.Rmd: produces the file Acne_Report.html. Collapses some descriptions with graphs and tables

Acne_Report.html: Output of Acne_Report.RMD

OHDSI_OMOP_Acne.Rproj: R project to work in all of these files

Cleaned/ folder:  Should be empty. Code in the "first step" file will populated this with cleaned data. If deleted, this code will break

Sensitivity Analysis folder: Contains a version of the raw data and report that exludes all visit concept IDs of 0. This is touched on in the limitations section of the Acne_Report.Rmd / Acne_Report.html. The results of this analysis agree with the full analysis.
