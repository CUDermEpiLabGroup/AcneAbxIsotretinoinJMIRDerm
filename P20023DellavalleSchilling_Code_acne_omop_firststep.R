#Project Title: P20023DellavalleSchilling - Acne Aim 1 description of cohort OMOP
#Date: August 24, 2020
#Analyst: M. Branda
#Purpose: Create patient cohort - ensure population provided by HDC meets eligibility criteria and create demographic variables.
#Program order:  Run this one first!


library(remotes)
library(devtools)
library(CohortMethod)
library(tidyverse)
library(lubridate)
library(psych)
library(summarytools)
library(stringr)
library(haven)
library(table1)
library (boot)
library(bigrquery)
library(dplyr)


#=== READING IN GOOGLE BIG QUERY DATA =========================

library(bigrquery)
project<- 'hdcomopacne'

#--- Read in Drug Exposure Table ------------------------------
sql_code <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.omop_acne.drug_exposure`'

drug_exposure_temp <- bq_project_query(project_name, sql_code)

drug_exposure <- bq_table_download(drug_exposure_temp, bigint = "character")

#--- Write a Copy Within Local Eureka Environment -------------
write.csv(drug_exposure, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/drug_exposure.csv")

################################################################

################################################################
#***code to read in data from bigquery!

project <- 'hdcomopacne'

Sql <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.project_logs.drug_exposure`'
test <- bq_project_query(project, Sql)
drug_exposure <- bq_table_download(test, bigint = "character")

# #saving a copy of the daa in csv to save time
write.csv(drug_exposure, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/drug_exposure.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.care_site`'
#care_site <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(care_site, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/care_site.csv")
################################################################
Sql <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.omop_acne.concept`'
concept <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

concept <- bq_download_retry(bq_project_query(project, Sql), 10000,  .tries = 15)

#saving a copy of the daa in csv to save time
write.csv(concept, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept.csv")
################################################################
Sql <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.omop_acne.concept_ancestor`'
concept_ancestor <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(concept_ancestor, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_ancestor.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.concept_class`'
#concept_class <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(concept_class, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_class.csv")
################################################################
# Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.concept_relationship`'
# concept_relationship <- bq_table_download(bq_project_query(project, Sql), bigint = "character")
# 
# #saving a copy of the daa in csv to save time
# write.csv(concept_relationship, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_relationship.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.concept_synonym`'
#concept_synonym <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(concept_synonym, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_synonym.csv")
################################################################
Sql <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.project_logs.condition_occurrence`'
condition_occurrence <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
write.csv(condition_occurrence, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/condition_occurrence.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.death`'
#death <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(death, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/death.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.device_exposure`'
#device_exposure <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(device_exposure, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/device_exposure.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.domain`'
#domain <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(domain, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/domain.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.drug_strength`'
#drug_strength <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(drug_strength, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/drug_strength.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.location`'
#location <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(location, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/location.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.measurement`'
#measurement <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(measurement, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/measurement.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.observation`'
#observation <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(observation, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/observation.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.observation_period`'
#observation_period <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(observation_period, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/observation_period.csv")
################################################################
Sql <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.project_logs.person`'
person <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
write.csv(person, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/person.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.procedure_occurrence`'
#procedure_occurrence <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(procedure_occurrence, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/procedure_occurrence.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.provider`'
#provider <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(provider, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/provider.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.relationship`'
#relationship <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(relationship, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/relationship.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.specimen`'
#specimen <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(actiondx, file="/home/grace.bosma/ncdr_dx_event_fact.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.visit_detail`'
#visit_detail <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(visit_detail, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/visit_detail.csv")
################################################################
Sql <- '#standardSQL
      SELECT *
      FROM `hdcomopacne.project_logs.visit_occurrence`'
visit_occurrence <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
write.csv(visit_occurrence, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/visit_occurrence.csv")
################################################################
#Sql <- '#standardSQL
#       SELECT *
#       FROM `hdcomopacne.omop_acne.vocabulary`'
#vocabulary <- bq_table_download(bq_project_query(project, Sql), bigint = "character")

#saving a copy of the daa in csv to save time
#write.csv(vocabulary, file="/home/grace.bosma/Documents/OHDSI_OMOP_Acne/vocabulary.csv")
################################################################

#Instead of big query saved out data to csv files and this allows you to read them in (it is quicker)
drug_exposure <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/drug_exposure.csv", stringsAsFactors=FALSE)
#care_site <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/care_site.csv", stringsAsFactors=FALSE)
concept <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept.csv", stringsAsFactors=FALSE)
#concept_ancestor <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_ancestor.csv", stringsAsFactors=FALSE)
#concept_class <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_class.csv", stringsAsFactors=FALSE)
concept_relationship <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_relationship.csv", stringsAsFactors=FALSE)
concept_synonym <- read.csv("/home/grace.bosma/Documents/OHDSI_OMOP_Acne/concept_synonym.csv", stringsAsFactors=FALSE)
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


#fixing some variables
person$birth_datetime <- as.Date(person$birth_datetime)
condition_occurrence$condition_start_date <- as.Date(condition_occurrence$condition_start_date)
condition_occurrence$condition_end_date <- as.Date(condition_occurrence$condition_end_date)
visit_occurrence$visit_start_date <- as.Date(visit_occurrence$visit_start_date)
visit_occurrence$visit_end_date <- as.Date(visit_occurrence$visit_end_date)


# collect(condition_occurrence)
# collect(drug_exposure)


condition_oc <- condition_occurrence %>%
  left_join(visit_occurrence, by=c("person_id", "visit_occurrence_id")) %>%
  #keep encounters within date range of protocol
  filter(visit_start_date >= "2015-01-01" & visit_start_date <= "2019-12-31") %>%
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
                                     40405454,40397376,44801557,44797199,40405447)) 

condition <- condition_oc %>%
  left_join(concept, by=c('condition_concept_id'='concept_id')) %>%
  rename(condition_concept_name=concept_name) %>%
  #if visit date is missing using condition start date
  select(person_id, visit_start_date, visit_type_concept_id, visit_occurrence_id, condition_concept_id, condition_concept_name, condition_start_date) 

#subset patient cohort to those that were flagged by HDC as included plus adding sanity check on birth date inclusiveness
personSubset <- person %>%
  #cohort 1 only for table 1 Study pop demographics
  filter(flag %in% c("1", "1|2"))  %>%
  #remove patients with birth dates outside of protocol range
  filter(birth_datetime >= "1974-05-31" & birth_datetime <= "2007-06-01") %>% 
  mutate(person_id = as.numeric(person_id))

#keeping first visit where patient is at least 10 years old (per protocol)
first_visit_of_interest <- condition %>%
  inner_join(personSubset) %>%
  mutate(date_diff=(as.numeric(difftime(visit_start_date,birth_datetime, units = "weeks")/52.25))) %>%
  filter(date_diff >= 10) %>%
  group_by(person_id) %>%
  arrange(visit_start_date) %>%
  slice(1L)

##getting the variouse concept information for patient demographics 
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

#only keeping those patients who meet all criteria and the merging in the demographic concepts. 
table1 <- personSubset %>%
  inner_join(first_visit_of_interest) %>%
  mutate(age_at_dx=round(as.numeric(difftime(visit_start_date,birth_datetime, units = "weeks")/52.25),0)) %>%
  #create age categories  
  mutate(agedx_cat=case_when(
    age_at_dx <= 15 ~ '10-15',
    age_at_dx >15 & age_at_dx <= 20 ~ '16 to 20',
    age_at_dx >20 & age_at_dx <= 25 ~ '21 to 25',
    age_at_dx >25 & age_at_dx <= 30 ~ '26 to 30',
    age_at_dx >30 & age_at_dx <= 35 ~ '31 to 35',
    age_at_dx >35 & age_at_dx <= 40 ~ '36 to 40',
    age_at_dx >40 & age_at_dx <= 45 ~ '41 to 45'
  )) %>%
  left_join(gender) %>%
  left_join(race) %>%
  left_join(ethnicity)


table1$gender <- factor(table1$gender, levels=c("FEMALE", "MALE"), labels=c("Female", "Male"))

label(table1$agedx_cat) <- "Age at first encounter"
label(table1$gender) <- "Gender"
label(table1$race) <- "Race"
label(table1$ethnicity) <- "Ethnicity"
label(table1$condition_concept_name) <- "Diagnosis"

table1(~agedx_cat + gender + race + ethnicity + condition_concept_name, data=table1)

