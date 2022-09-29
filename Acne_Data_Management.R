###########################################################################################
# Project: P20023DellavalleSchilling - Acne Aim 1 description of cohort OMOP
# Date: November 2021
# Analyst: G. Bosma (with code inhereted from M. Branda)
# Purpose: Create my own flow that makes more sense to me
# Program Order: Can only run this after running Megan's First Step Code. 
###########################################################################################

#=== SETUP =================================================================================
library(tidyverse)

drug_exposure <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/drug_exposure.csv", stringsAsFactors=FALSE)
#care_site <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/care_site.csv", stringsAsFactors=FALSE)
concept <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept.csv", stringsAsFactors=FALSE)
#concept_ancestor <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_ancestor.csv", stringsAsFactors=FALSE)
#concept_class <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_class.csv", stringsAsFactors=FALSE)
#concept_relationship <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_relationship.csv", stringsAsFactors=FALSE)
#concept_synonym <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_synonym.csv", stringsAsFactors=FALSE)
condition_occurrence <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/condition_occurrence.csv", stringsAsFactors=FALSE)
#death <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/death.csv", stringsAsFactors=FALSE)
#device_exposure <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/device_exposure.csv", stringsAsFactors=FALSE)
#domain <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/domain.csv", stringsAsFactors=FALSE)
#drug_strength <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/drug_strength.csv", stringsAsFactors=FALSE)
#location <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/location.csv", stringsAsFactors=FALSE)
#measurement <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/measurement.csv", stringsAsFactors=FALSE)
#observation <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/observation.csv", stringsAsFactors=FALSE)
#observation_period <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/observation_period.csv", stringsAsFactors=FALSE)
person <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/person.csv", stringsAsFactors=FALSE)
#procedure_occurrence <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/procedure_occurrence.csv", stringsAsFactors=FALSE)
#provider <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/provider.csv", stringsAsFactors=FALSE)
#relationship <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/relationship.csv", stringsAsFactors=FALSE)
#visit_detail <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/visit_detail.csv", stringsAsFactors=FALSE)
visit_occurrence <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/visit_occurrence.csv", stringsAsFactors=FALSE)
#vocabulary <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/vocabulary.csv", stringsAsFactors=FALSE)

atlas_amoxicillin <- read.csv("ATLAS/Acne ATLAS Medicine Codes _Amoxicillin Combo Oral_Schilling_209.csv")
atlas_azithromycin <- read.csv("~/Documents/OHDSI_OMOP_Acne/ATLAS/Acne ATLAS Medicine Codes _Azithromycin Oral_Schilling_210.csv")
atlas_cephalexin <- read.csv("~/Documents/OHDSI_OMOP_Acne/ATLAS/Acne ATLAS Medicine Codes _Cephalexin Oral_Schilling_211.csv")
atlas_doxycycline <- read.csv("~/Documents/OHDSI_OMOP_Acne/ATLAS/Acne ATLAS Medicine Codes _Doxycycline Oral_Schilling_208.csv")
atlas_isotretinoin <- read.csv("~/Documents/OHDSI_OMOP_Acne/ATLAS/Acne ATLAS Medicine Codes _Isotretinoin Oral_Schilling_207.csv")
atlas_minocycline <- read.csv("~/Documents/OHDSI_OMOP_Acne/ATLAS/Acne ATLAS Medicine Codes _Minocycline Oral_Schilling_205.csv")
atlas_sufamethoxazole <- read.csv("~/Documents/OHDSI_OMOP_Acne/ATLAS/Acne ATLAS Medicine Codes _Sulfamethoxazole Combo_Schilling_212.csv")

#=== CLEANING ================================================================================

#--- Variable Types -------------------------------------------------------------------------- 
person$birth_datetime <- as.Date(person$birth_datetime)
condition_occurrence$condition_start_date <- as.Date(condition_occurrence$condition_start_date)
condition_occurrence$condition_end_date <- as.Date(condition_occurrence$condition_end_date)
visit_occurrence$visit_start_date <- as.Date(visit_occurrence$visit_start_date)
visit_occurrence$visit_end_date <- as.Date(visit_occurrence$visit_end_date)
visit_occurrence$person_id <- as.character(visit_occurrence$person_id)

#--- Eligible Individuals --------------------------------------------------------------------

gender <- concept %>%
  rename(gender=concept_name, gender_concept_id=concept_id)  %>%
  filter(domain_id == "Gender") %>%
  select(gender, gender_concept_id)

race <- concept %>%
  rename(race=concept_name, race_concept_id=concept_id) %>%
  filter(domain_id == 'Race') %>%
  select(race, race_concept_id)

ethnicity <- concept %>%
  rename(ethnicity=concept_name, ethnicity_concept_id=concept_id) %>%
  filter(domain_id == 'Ethnicity') %>%
  select(ethnicity, ethnicity_concept_id)

# 16,219 individuals in raw person dataset to 15,794 after birthdate filter
personSubset <- person %>%
  #remove patients with birth dates outside of protocol range
  # places them between the age of 10 and 45 as of 1/1/2015 (inclusive)
  filter(birth_datetime >= "1969-01-02" & birth_datetime <= "2005-01-01") %>% 
  mutate(Age = as.numeric(round(difftime("2015-01-01", birth_datetime, units = "weeks")/52.25,0))) %>% 
  mutate(person_id = as.character(person_id)) %>% 
  left_join(gender) %>%
  left_join(race) %>% 
  left_join(ethnicity) %>% 
  mutate(gender = factor(gender, levels = c("FEMALE", "MALE"), labels = c("Female", "Male")),
         race = factor(race), 
         ethnicity = factor(ethnicity))

excluded_age <- person %>%
  #remove patients with birth dates outside of protocol range
  # places them between the age of 10 and 45 as of 1/1/2015 (inclusive)
  filter(birth_datetime < "1969-01-02" | birth_datetime > "2005-01-01") %>% 
  mutate(weeks_exact = as.numeric(difftime("2015-01-01", birth_datetime, units = "weeks"))) %>%
  mutate(Age = round(weeks_exact/52.25,0))
  

# there are 133 people filtered with flag of 2????
# personSubset %>% filter(flag == 2)
# could this be to exclude those with a diagnoses of RMSF, chlamydia, syphilis, malaria...
# the short list of exclusions in the example manuscript?


#--- Eligible Visits -------------------------------------------------------------

# connect concept names to the conditions, keep only those that are marked as acne
# merge with visit information 
# 371999 ppl after this step
condition_oc <- condition_occurrence %>%
  left_join(concept, by=c('condition_concept_id'='concept_id')) %>%
  rename(condition_concept_name=concept_name) %>% 
  mutate(person_id = as.character(person_id)) %>% 
  #keep only conditions of interest for acne, list pulled from atlas based off concepts
  filter(condition_concept_id %in% c(141095,4033342,40397373,4218760,4172185,4066164,
                                     4299124,4306449,40397372,40448871,40397357,4263196,
                                     4296354,4033510,4299558,4224749,4314433,4299803,
                                     4047261,40397375,4260680,4291607,4299706,4042209,
                                     4291288,40331541,40352948,4106787,4201752,4294245,
                                     40319766,4063564,138826,40397355,40397358,4150181,
                                     4173157,4227594,40352968,40319827,4299557,4242113,
                                     4201580,4066930,4009032,4307674,40397362,4067180,
                                     4032880,4028013,4159143,40578312,4004094,4208828,
                                     4071708,4299705,40397365,4177852,4136207,4066931,
                                     4032879,4080922,4172307,4032881,4067181,4299704,
                                     4063563,4065351,4296355,4243513,40397368,4300097,
                                     4065350,4293591,4300923,4300924,4033023,40397370,
                                     40405454,40397376,44801557,44797199,40405447)) %>% 
  dplyr::select(person_id, condition_start_date, condition_type_concept_id, condition_occurrence_id, condition_concept_id, condition_concept_name, visit_occurrence_id) %>% 
  left_join(visit_occurrence, by=c("person_id", "visit_occurrence_id")) 

condition <- condition_oc %>% 
  #keep encounters within date range of protocol - removes 40707 people 
  filter(visit_start_date >= "2003-01-01" & visit_start_date <= "2019-12-31")%>% 
  #keep those with two diagnoses of Acne within time range above - removes 13 people
  group_by(person_id) %>% 
  filter(n() >= 2)

#--- Eligible Medications --------------------------------------------------------

drug_ex <- drug_exposure %>% 
  
  filter(drug_exposure$drug_concept_id %in% atlas_amoxicillin$Id | 
         drug_exposure$drug_concept_id %in% atlas_azithromycin$Id | 
         drug_exposure$drug_concept_id %in% atlas_cephalexin$Id |
         drug_exposure$drug_concept_id %in% atlas_doxycycline$Id | 
         drug_exposure$drug_concept_id %in% atlas_minocycline$Id | 
         drug_exposure$drug_concept_id %in% atlas_sufamethoxazole$Id
          ) %>% 
  
  left_join(select(concept, c("concept_id", "concept_name")), by=c("drug_concept_id" = "concept_id")) %>% 
  
  filter(!grepl('liquid|suspension|powder|foam', ignore.case = TRUE, concept_name))
  
  
  
# dummy variables 

drug_ex$amoxicillian <- ifelse(drug_ex$drug_concept_id %in% atlas_amoxicillin$Id,1, 0)
drug_ex$azithromycin <- ifelse(drug_ex$drug_concept_id %in% atlas_azithromycin$Id,1, 0)
drug_ex$cephalexin <- ifelse(drug_ex$drug_concept_id %in% atlas_cephalexin$Id ,1, 0)
drug_ex$doxycycline <- ifelse(drug_ex$drug_concept_id %in% atlas_doxycycline$Id,1, 0)
drug_ex$minocycline <- ifelse(drug_ex$drug_concept_id %in% atlas_minocycline$Id,1, 0)
drug_ex$sulfamethoxazole <-ifelse(drug_ex$drug_concept_id %in% atlas_sufamethoxazole$Id,1, 0)

drug_ex$rx <- ifelse(drug_ex$drug_concept_id %in% atlas_amoxicillin$Id, "Amoxicillin",
           ifelse(drug_ex$drug_concept_id %in% atlas_azithromycin$Id, "Azithromycin",
           ifelse(drug_ex$drug_concept_id %in% atlas_cephalexin$Id, "Cephalexin",  
           ifelse(drug_ex$drug_concept_id %in% atlas_doxycycline$Id, "Doxycycline",
           ifelse(drug_ex$drug_concept_id %in% atlas_minocycline$Id, "Minocycline",  
           ifelse(drug_ex$drug_concept_id %in% atlas_sufamethoxazole$Id, "Sufamethoxazole", NA))))))
                                                             
  
# Filter quantity 
#megans_table <- drug_ex %>% select(drug_concept_id, concept_name, quantity) %>% unique

# filter regex for liquids / solution
# pull out differneces between megans' original table and my version

standards_full <- readxl::read_excel("Acne med and dosing RD x'ed medication_dosinginfo.xlsx", 
                                sheet = "Medication and quantity supplie")
standards <- standards_full %>% 
  filter(!is.na(...4))%>% 
  group_by(drug_concept_id, drug_concept_name) %>% 
  summarize(limit = max(quantity))

# standards question 
# megans_table_differences <- megans_table %>%
#   
#   filter(!(megans_table$drug_concept_id %in% standards$drug_concept_id)) %>% 
#   
#   left_join(select(drug_ex, drug_concept_id, quantity)) %>% 
#   
#   group_by(drug_concept_id) %>% 
#   
#   unique() %>% 
#   
#   arrange(drug_concept_id, quantity)

# minocycline question
# only_mino <- drug %>% filter(minocycline == 1) %>% 
#   select(drug_concept_id, concept_name) %>% 
#   arrange(drug_concept_id) %>% 
#   unique()


drug <- drug_ex %>% 
  
  left_join(standards) %>%
  
  mutate(limit = replace_na(limit, 27)) %>%  # if not explicately listed, assume 27
  
  filter(quantity > limit)  %>% # too small to be for acne

  mutate(person_id = as.character(person_id))
  

#--- Drug: ISOT -----------------------------------------------------------------
drug_ex_ISO <- drug_exposure %>% 
  
  filter(drug_exposure$drug_concept_id %in% atlas_isotretinoin$Id) %>% 
  
  mutate(isotretinoin = 1) %>% 
  
  left_join(select(concept, c("concept_id", "concept_name")), by=c("drug_concept_id" = "concept_id"))


drug_ISO <- drug_ex_ISO %>% 
  
  left_join(standards) %>%
  
  mutate(limit = replace_na(limit, 27)) %>%  # if not explicately listed, assume 27
  
  filter(quantity > limit)  %>% 
  
  mutate(person_id = as.character(person_id))


#=== MERGING =====================================================================

# merge list of eligibile aged people with eligible diagnosed people
eligible_ppl <- personSubset %>% 
  inner_join(condition, by=c('person_id')) %>% 
  select(flag, person_id, birth_datetime, Age, gender, race, ethnicity,
         condition_start_date, condition_occurrence_id, condition_concept_id, 
         condition_concept_name, visit_occurrence_id, visit_concept_id, 
         visit_start_date, visit_type_concept_id, 
         )

# left join with eligibile people
# this keeps those with acne diag & age in inclusion 
# and those who did not get medications
eligible_all <- eligible_ppl %>% 
  left_join(drug, by=c('visit_occurrence_id', 'person_id'))%>% 
  # keep first eligible date of acne dx 
  group_by(person_id) %>% 
  select(!condition_occurrence_id) %>% 
  unique() %>% 
  mutate(num_elig_visits = n(), 
         visits_w_med_rx = sum(!is.na(drug_concept_id)), 
         first_acne_diag = min(visit_start_date), 
         amoxicillian= sum(amoxicillian, na.rm = TRUE), 
         azithromycin = sum(azithromycin, na.rm = TRUE), 
         cephalexin = sum(cephalexin, na.rm = TRUE), 
         doxycycline = sum(doxycycline, na.rm = TRUE), 
         minocycline = sum(minocycline, na.rm = TRUE), 
         sulfamethoxazole = sum(sulfamethoxazole, na.rm = TRUE), 
         med_filled = ifelse(visits_w_med_rx > 0, "Received Medication", "None"), 
         Age_10yr = Age /10
         ) %>% 
  arrange(condition_start_date, visit_start_date) 


byperson <- eligible_all %>% 
  slice(1L)


# test <- eligible_all[which(eligible_all$refills == 99), ]

eligible_all_ISO <- eligible_ppl %>% 
  left_join(drug_ISO, by=c('visit_occurrence_id', 'person_id'))%>% 
  # keep first eligible date of acne dx 
  group_by(person_id) %>% 
  select(!condition_occurrence_id) %>% 
  unique() %>% 
  mutate(num_elig_visits = n(), 
         visits_w_med_rx = sum(!is.na(drug_concept_id)), 
         first_acne_diag = min(visit_start_date), 
         med_filled = ifelse(visits_w_med_rx > 0, "Received Medication", "None"), 
         Age_10yr = Age /10
  ) %>% 
  arrange(condition_start_date, visit_start_date) 

byperson_ISO <- eligible_all_ISO %>% 
  slice(1L)


#=== WRITE =======================================================================
 
write.csv(byperson, file = "cleaned/byperson.csv")
write.csv(byperson_ISO, file = "cleaned/byperson_ISO.csv")
write.csv(eligible_all, file = "cleaned/eligible_all.csv")
write.csv(eligible_all_ISO, file = "cleaned/eligible_all_ISO.csv")

#write.csv(megans_table_differences, file = "megans_table_differences.csv")
#write.csv(only_mino, file = "only_mino.csv")

# The below code may be helpful if ever to revisit this. The way this is coded is not 
# efficient in terms of space / filtering. Below is a case example of code that would have
# been helpful when beginning this project. project names etc will have to be updated.

#===JONATHAN CODE 
# 
# library(tidyverse)
# library(DBI)
# 
# 
# # Establish a connection with the DB
# project = "sandbox-omop"
# dataset = "CHCO_DeID_Apr2018"
# conn = dbConnect(
#   bigrquery::bigquery(),
#   project = project,
#   dataset = dataset
# )
# 
# # r Explore tables
# dbListTables(conn)
# 
# # Query Tables
# ptable = tbl(conn, "person") %>% 
#   select(person_id, care_site_id, location_id, year_of_birth, gender_source_value, race_concept_id, race_source_concept_id) %>% 
#   filter(year_of_birth > 2009) %>% 
#   arrange(desc(year_of_birth))
# 
# dtable = tbl(conn, "drug_exposure") %>% 
#  select(person_id, drug_exposure_start_datetime, drug_exposure_end_datetime, drug_concept_id, drug_source_value) %>% 
#   filter(str_detect(drug_source_value, "(?i)fluticasone|prednisone"))
# 
# joinedTables = ptable %>% 
#   inner_join(dtable, by = "person_id")
# 
# joinedTables %>% 
#   head()
# 
# 
# # OMOP Tips
# # Look for medication hierarchies -- VA Class, NDFRT and Anatomical Therapeutic Chemical (ATC)
# tbl(conn, "vocabulary") %>% 
#   arrange(vocabulary_name)
# 
# 
# # Check concept table columns
# tbl(conn, "concept") %>% 
#   colnames(.)
# 
# 
# # High level medications
# # Pull seizure/epilepsy classification concepts from ATC
# tbl(conn, "concept") %>% 
#   filter(vocabulary_id == "ATC" & str_detect(concept_name, "(?i)epile|seiz")) %>% 
#   select(concept_id, concept_name)
# 
# # Map high level medications to lower level medications
# # Use Ancestor Hierarchy to get down to terminal drug nodes (RxNorm codes)
# epilepsy_concepts = tbl(conn, "concept") %>% 
#   filter(vocabulary_id == "ATC" & str_detect(concept_name, "(?i)epile|seiz")) %>% 
#   select(concept_id) %>% 
#   collect()
# 
# hierarchical_mapping = tbl(conn, "concept_ancestor") %>% 
#   inner_join(
#     tbl(conn, "concept"),
#     by = c("ancestor_concept_id"="concept_id")
#   ) %>% 
#   inner_join(
#     tbl(conn, "concept") %>% filter(vocabulary_id == "RxNorm"), # You can map to lower level ATC (less granular than RxNorm)
#     by = c("descendant_concept_id"="concept_id")
#   ) %>% 
#   filter(ancestor_concept_id %in% c(!!epilepsy_concepts$concept_id)) %>% 
#   select(ancestor_concept_id, concept_name_x, descendant_concept_id, concept_name_y) %>% 
#   rename(ancestor_concept = concept_name_x, descendant_concept = concept_name_y) %>% 
#   collect()
# 
# View(hierarchical_mapping)
