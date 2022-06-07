setwd("C:/Users/tstoker/Desktop/RWD")
rm(list=ls())
library(dplyr)
library(ggplot2)
library(raster)
library(tidyquant)
library(tidyr)
library(reactable)
library(plotly)
library(stringr)
library(lubridate)
library(zoo)

#######################################
# Import Data, must change file names #0

#######################################


#Import yesterday's cases
yesterday_cases <- read.csv("yesterday_cases.csv", header = T) %>% 
  dplyr::select(-1) %>% 
  rename(`Week Of` = Week.Of)

export_epicases <- read.csv("epicases67.csv", header = T)
export_cases <- read.csv("cases67.csv", header = T)
export_daytests <- read.csv("testsday67.csv", header = T)
export_tests <- read.csv("tests67.csv", header = T)
export_deaths <- read.csv("deaths67.csv", header = T)
export_vax <- read.csv("vax67.csv")



##############
# Clean Data #
##############

###Format dates in all data sets
#Day Tests
export_daytests$case_date <- as.Date(export_daytests$case_date)
#Tests
export_tests$collect_date_fillrph <- as.Date(export_tests$collect_date_fillrph)
#Vaccine
export_vax$vaccine_date <- as.Date(export_vax$vaccine_date)
#Case
export_cases$case_onset_date <- as.Date(export_cases$case_onset_date)
export_cases$patient_first_reported_ph_date <- as.Date(export_cases$patient_first_reported_ph_date)
export_cases$date_of_death <- as.Date(export_cases$date_of_death)
export_cases$first_positive_lab <- as.Date(export_cases$first_positive_lab)
export_cases$first_created_lab_date <- as.Date(export_cases$first_created_lab_date)
export_cases$case_date <- as.Date(export_cases$case_date)
export_cases$onset_date <- as.Date(export_cases$onset_date)
export_cases$caseweekdate <- as.Date(export_cases$caseweekdate)
export_cases$vaccine_administered_date <- as.Date(export_cases$vaccine_administered_date)
export_cases$vaccine_administered_date.2 <- as.Date(export_cases$vaccine_administered_date.2)
export_cases$vac_complete_date <- as.Date(export_cases$vac_complete_date)
export_cases$wgs_collection_date <- as.Date(export_cases$wgs_collection_date)
export_cases$date_of_designation <- as.Date(export_cases$date_of_designation)
export_cases$hosp_admit_date <- as.Date(export_cases$hosp_admit_date)
#Death
export_deaths$case_onset_date <- as.Date(export_deaths$case_onset_date)
export_deaths$patient_first_reported_ph_date <- as.Date(export_deaths$patient_first_reported_ph_date)
export_deaths$date_of_death <- as.Date(export_deaths$date_of_death)
export_deaths$first_positive_lab <- as.Date(export_deaths$first_positive_lab)
export_deaths$first_created_lab_date <- as.Date(export_deaths$first_created_lab_date)
export_deaths$case_date <- as.Date(export_deaths$case_date)
export_deaths$onset_date <- as.Date(export_deaths$onset_date)
export_deaths$caseweekdate <- as.Date(export_deaths$caseweekdate)
export_deaths$vaccine_administered_date <- as.Date(export_deaths$vaccine_administered_date)
export_deaths$vaccine_administered_date.2 <- as.Date(export_deaths$vaccine_administered_date.2)
export_deaths$vac_complete_date <- as.Date(export_deaths$vac_complete_date)
export_deaths$wgs_collection_date <- as.Date(export_deaths$wgs_collection_date)
export_deaths$date_of_designation <- as.Date(export_deaths$date_of_designation)
#EpiTrax Cases
export_epicases$covid_case_date <- as.Date.character(export_epicases$covid_case_date)
export_epicases$patient_date_of_death <- as.Date.character(strsplit(as.character(strptime(export_epicases$patient_date_of_death,"%Y-%m-%d"))," "))


###Clean Case Data

export_cases <- export_cases %>% 
  mutate(case_week = floor_date(case_date, unit = "week", week_start = 7)) %>% 
  mutate(hosp_week = floor_date(hosp_admit_date, unit = "week", week_start = 7)) %>% 
  mutate(county_name = case_when(county == 2 ~ "Box Elder",
                                county == 3 ~ "Cache",
                                county == 17 ~ "Rich")) %>% 
  replace(is.na(.),"") 
  

###Clean Death Data

export_deaths <- export_deaths %>% 
  mutate(death_week = floor_date(date_of_death, unit = "week", week_start = 7)) %>% 
  mutate(death_month = floor_date(case_date, unit = "month")) %>%
  mutate(county_name = case_when(county == 2 ~ "Box Elder",
                                 county == 3 ~ "Cache",
                                 county == 17 ~ "Rich")) %>% 
  replace(is.na(.),"")


###Clean EpiTrax Case Data

cleaned_epicases <- export_epicases %>% 
  filter(patient_jurisdiction_of_residence %in% c("Bear River","")) %>% 
  filter(patient_state_case_status == "Confirmed" | condition_caused_death == "Yes") %>% 
  #Removes duplicate CMRs, prioritizing CMRs with "Inpatient" visit type over "", "Unknown", or "Outpatient"
  arrange(factor(person_facility_visit_type, levels = c("Inpatient", "Outpatient", "Unknown", NA))) %>% 
  distinct(patient_record_number, .keep_all = TRUE) %>% 
  #Creates week variable using epiweek, where a week is Sun-Sat
  mutate(week_of = floor_date(covid_case_date, unit = "week", week_start = 7)) %>% 
  mutate(agecat = case_when(patient_age_at_event_in_years < 10 ~ "0-9",
                            patient_age_at_event_in_years < 18 ~ "10-17",
                            patient_age_at_event_in_years < 26 ~ "18-25",
                            patient_age_at_event_in_years < 36 ~ "26-35",
                            patient_age_at_event_in_years < 46 ~ "36-45",
                            patient_age_at_event_in_years < 56 ~ "46-55",
                            patient_age_at_event_in_years < 66 ~ "56-65",
                            patient_age_at_event_in_years < 76 ~ "66-75",
                            patient_age_at_event_in_years < 86 ~ "76-85",
                            patient_age_at_event_in_years < 120 ~ "86+",
                            TRUE ~ "Unknown"))

#Clean City Data
cleaned_epicases <- cleaned_epicases %>% 
  mutate(city = if_else(str_detect(address_at_diagnosis_city, "Bear|BEAR"), "Bear River City",
                        if_else(str_detect(address_at_diagnosis_city, "Brigham|BRIGHAM"), "Brigham City",
                                if_else(str_detect(address_at_diagnosis_city, "college"), "College Ward",
                                        if_else(str_detect(address_at_diagnosis_city, "Corinne|CORINNE|Corrine"), "Corinne",
                                                if_else(str_detect(address_at_diagnosis_city, "HYDE"), "Hyde Park",
                                                        if_else(str_detect(address_at_diagnosis_city, "HYRUM"), "Hyrum",
                                                                if_else(str_detect(address_at_diagnosis_city, "LAKETOWN"), "Laketown",
                                                                        if_else(str_detect(address_at_diagnosis_city, "NORTH LOGAN"), "North Logan",
                                                                                if_else(str_detect(address_at_diagnosis_city, "Loagn|logan|LOGAN"), "Logan",
                                                                                        if_else(str_detect(address_at_diagnosis_city, "PERRY"), "Perry",
                                                                                                if_else(str_detect(address_at_diagnosis_city, "RICHMOND"), "Richmond",
                                                                                                        if_else(str_detect(address_at_diagnosis_city, "RIVER HEIGHTS"), "River Heights",
                                                                                                                if_else(str_detect(address_at_diagnosis_city, "SANTAQUIN"), "Santaquin",
                                                                                                                        if_else(str_detect(address_at_diagnosis_city, "WELLSVILLE"), "Wellsville",
                                                                                                                                if_else(str_detect(address_at_diagnosis_city, "willard"), "Willard", as.character(address_at_diagnosis_city)
                                                                                                                                )))))))))))))))) %>% 
  replace(is.na(.),"")

###Clean Test Data

export_daytests <- export_daytests %>% 
  filter(test_result %in% c("POSITIVE","NEGATIVE")) %>%
  filter(test_type == "PCR/amplification") %>% 
  replace(is.na(.),"")

export_tests <- export_tests %>% 
  filter(test_result %in% c("POSITIVE","NEGATIVE")) %>%
  filter(test_type == "PCR/amplification") %>% 
  replace(is.na(.),"")


###Clean Vaccine Data

#Mutate vax export to align better with census data
cleaned_vax <- export_vax %>% 
  #Combines zip codes 84322 and 84323 (On-campus USU residence and PO box, respectively) with 84321, so we can apply it to population estimates
  mutate(zipcode_map = ifelse(zipcode %in% c(84322,84323), 84321, zipcode),
         #Create combined race_eth variable, which takes the ethnicity column if a patient is Hispanic/Latino, and race column if they are not
         race_eth = ifelse(ethnicity == "Hispanic or Latino", "Hispanic or Latino", as.character(race))) %>% 
  replace(is.na(.),"")



###################################################################################
# Create Panel Data Sets for Death, Hospitalization, Case, Test, and Vaccine Data #
###################################################################################

###Prep date values that help with the creation of each panel data frame

todays_date <- Sys.Date()-1
todays_testdate <- Sys.Date()-6
todays_vaxdate <- Sys.Date()-1

###Set Color Palette
BRHD_cols = list(rgb(0, 141, 168, maxColorValue = 255), rgb(241, 227, 197, maxColorValue = 255),
                 rgb(212, 69, 29, maxColorValue = 255), rgb(102, 51, 52, maxColorValue = 255),
                 rgb(255, 206, 113, maxColorValue = 255), rgb(109, 39, 106, maxColorValue = 255),
                 rgb(231, 65, 122, maxColorValue = 255))

###Death Data

#Group by week
death_week <- bind_cols(week_of = rep(seq(as.Date("2020-03-29"),todays_date,by=7),1))

death_week <- left_join(death_week, export_deaths %>%
                          rename(week_of = death_week) %>% 
                          group_by(week_of) %>% 
                          summarize(weekly_deaths = sum(patient_died == "Yes"), 
                                    vaccinated = sum(breakthrough == "Yes"), 
                                    unvaccinated = sum(breakthrough == "No"),
                                    Cache = sum(county_name == "Cache"),
                                    `Box Elder` = sum(county_name == "Box Elder"),
                                    Rich = sum(county_name == "Rich"),
                                    `<1`= sum(age == "0-1"), 
                                    `1-14`= sum(age == "1-14"),
                                    `15-24`= sum(age == "15-24"),
                                    `25-44`= sum(age == "25-44"), 
                                    `45-64`= sum(age == "45-64"), 
                                    `65-84`= sum(age == "65-84"), 
                                    `85+` = sum(age == "85+"))) %>%
  #replace NA's with zeros, create mutated variables from variables created in the step above          
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         total_deaths = cumsum(weekly_deaths),
         Cache_total = cumsum(Cache),
         Box_Elder_total = cumsum(`Box Elder`),
         Rich_total = cumsum(Rich),
         deaths_4wa = round(SMA(weekly_deaths, 4),1))

#Group by month
death_month <- bind_cols(Month = seq(min(export_deaths$death_month),max(export_deaths$death_month), by="month"))

death_month <- left_join(death_month, export_deaths %>%
                          rename(Month = death_month) %>% 
                          group_by(Month) %>% 
                          summarize(monthly_deaths = sum(patient_died == "Yes"), 
                                    vaccinated = sum(breakthrough == "Yes"), 
                                    unvaccinated = sum(breakthrough == "No"))) %>% 
  #replace NA's with zeros, create mutated variables from variables created in the step above          
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         vac_perc = round(vaccinated/monthly_deaths,2),
         unvax_perc = round(unvaccinated/monthly_deaths,2))



###Hospitalizations
hosp <- export_cases %>% 
  filter(hosp == "Yes")

#Build panel dataset
hosp_week <- bind_cols(week_of = rep(seq(as.Date("2020-03-15"),todays_date,by=7),1))

hosp_week <- left_join(hosp_week, hosp %>%
                          rename(week_of = hosp_week) %>% 
                          group_by(week_of) %>% 
                          summarize(weekly_hosp = sum(hosp == "Yes"), 
                                    vaccinated = sum(breakthrough == "Yes"), 
                                    unvaccinated = sum(breakthrough == "No"),
                                    Cache = sum(county_name == "Cache"),
                                    `Box Elder` = sum(county_name == "Box Elder"),
                                    Rich = sum(county_name == "Rich"),
                                    `<1`= sum(age == "0-1"), 
                                    `1-14`= sum(age == "1-14"),
                                    `15-24`= sum(age == "15-24"),
                                    `25-44`= sum(age == "25-44"), 
                                    `45-64`= sum(age == "45-64"), 
                                    `65-84`= sum(age == "65-84"), 
                                    `85+` = sum(age == "85+"))) %>%
  #replace NA's with zeros, create mutated variables from variables created in the step above          
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         total_hosp = cumsum(weekly_hosp),
         Cache_total = cumsum(Cache),
         Box_Elder_total = cumsum(`Box Elder`),
         Rich_total = cumsum(Rich),
         hosp_4wa = round(SMA(weekly_hosp, 4),1))


###Cases
case_daily <- bind_cols(Date = seq.Date(as.Date("2020-03-01"),todays_date,by=1))

case_daily <- left_join(case_daily, export_cases %>%
                         rename(Date = case_date) %>% 
                         group_by(Date) %>% 
                         summarize(BRHD_new = sum(st_case_status == "Confirmed"),
                                   vaccinated = sum(breakthrough == "Yes"), 
                                   unvaccinated = sum(breakthrough == "No"),
                                   Cache_new = sum(county_name == "Cache"),
                                   Box_Elder_new = sum(county_name == "Box Elder"),
                                   Rich_new = sum(county_name == "Rich"),
                                   reinfection = sum(reinfection == "Yes"))) %>% 
  #replace NA's with zeros, create mutated variables from variables created in the step above          
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         BRHD_total = cumsum(BRHD_new),
         Cache_total = cumsum(Cache_new),
         Box_Elder_total = cumsum(Box_Elder_new),
         Rich_total = cumsum(Rich_new),
         BRHD_7d = round(SMA(BRHD_new, 7),1),
         Cache_7d = round(SMA(Cache_new, 7),1),
         Box_Elder_7d = round(SMA(Box_Elder_new, 7),1),
         Rich_7d = round(SMA(Rich_new, 7),1),
         nonreinfection = BRHD_new - reinfection,
         vac_perc = vaccinated/BRHD_new,
         unvax_perc = unvaccinated/BRHD_new,
         reinf_perc = reinfection/BRHD_new,
         nonreinf_perc = nonreinfection/BRHD_new,
         BRHD_pop = 189463,
         Box_Elder_pop = 57007,
         Cache_pop = 130004,
         Rich_pop = 2452,
         BRHD_pc = round(BRHD_new*100000/BRHD_pop,2),
         Box_Elder_pc = round(Box_Elder_new*100000/Box_Elder_pop,2),
         Cache_pc = round(Cache_new*100000/Cache_pop,2),
         Rich_pc = round(Rich_new*100000/Rich_pop,2),
         BRHD_pc7 = round(SMA(BRHD_pc, 7),1),
         Box_Elder_pc7 = round(SMA(Box_Elder_pc, 7),1),
         Cache_pc7 = round(SMA(Cache_pc, 7),1),
         Rich_pc7 = round(SMA(Rich_pc, 7),1),
         BRHD_7rate = rollapply(BRHD_new, 7,sum,align="right",fill=NA),
         Cache_7rate = rollapply(Cache_new, 7,sum,align="right",fill=NA),
         Box_Elder_7rate = rollapply(Box_Elder_new, 7,sum,align="right",fill=NA),
         Rich_7rate = rollapply(Rich_new, 7,sum,align="right",fill=NA),
         BRHD_pcrate = round(BRHD_7rate*1000/BRHD_pop,2),
         Box_Elder_pcrate = round(Box_Elder_7rate*1000/Box_Elder_pop,2),
         Cache_pcrate = round(Cache_7rate*1000/Cache_pop,2),
         Rich_pcrate = round(Rich_7rate*1000/Rich_pop,2))

#Cases by Week
case_week <- bind_cols(week_of = rep(seq(as.Date("2020-03-29"),todays_date-7,by=7),1))

case_week <- left_join(case_week, export_cases %>%
                         rename(week_of = case_week) %>% 
                         group_by(week_of) %>% 
                         summarize(BRHD_new_week = sum(st_case_status == "Confirmed"), 
                                   vaccinated = sum(breakthrough == "Yes"), 
                                   unvaccinated = sum(breakthrough == "No"),
                                   Cache_new_week = sum(county_name == "Cache"),
                                   Box_Elder_new_week = sum(county_name == "Box Elder"),
                                   Rich_new_week = sum(county_name == "Rich"),
                                   reinfection = sum(reinfection == "Yes"),
                                   `<1`= sum(age == "0-1"), 
                                   `1-14`= sum(age == "1-14"),
                                   `15-24`= sum(age == "15-24"),
                                   `25-44`= sum(age == "25-44"), 
                                   `45-64`= sum(age == "45-64"), 
                                   `65-84`= sum(age == "65-84"), 
                                   `85+` = sum(age == "85+"))) %>%
  #replace NA's with zeros, create mutated variables from variables created in the step above          
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         vac_perc = round(vaccinated/BRHD_new_week,2),
         unvax_perc = round(unvaccinated/BRHD_new_week,2),
         firstinfection = BRHD_new_week - reinfection,
         reinf_perc = round(reinfection/BRHD_new_week,2),
         firstinf_perc = round(firstinfection/BRHD_new_week,2),
         BRHD_4wa = round(SMA(BRHD_new_week, 4),1))

###Cases City

max(export_epicases$covid_case_date)
#Create dataframe shell
BRHD_city <- bind_cols(Date = seq(as.Date("2020-03-01"),max(cleaned_epicases$covid_case_date),by=1),
                       brigham_pop = 19601,
                       garland_pop = 2590,
                       hydepark_pop = 4797,
                       hyrum_pop = 8619,
                       lewiston_pop = 1798,
                       logan_pop = 51542,
                       mendon_pop = 1396,
                       millville_pop = 2150,
                       northlogan_pop = 11237,
                       paradise_pop = 1022,
                       perry_pop = 5248,
                       providence_pop = 7780,
                       richmond_pop = 2803,
                       riverheights_pop = 2076,
                       smithfield_pop = 12025,
                       tremonton_pop = 9206,
                       wellsville_pop = 3941,
                       willard_pop = 1958)


#Merge shell with case data to fill daily cases by region and age category 
BRHD_city <- left_join(BRHD_city, cleaned_epicases %>%
                         filter(covid_case_date <= todays_date) %>%
                         group_by(covid_case_date) %>% 
                         rename(Date = covid_case_date) %>% 
                         summarize(brigham = sum(address_at_diagnosis_city == "Brigham City"), 
                                   garland = sum(address_at_diagnosis_city == "Garland"),
                                   hyrum = sum(address_at_diagnosis_city == "Hyrum"),
                                   lewiston = sum(address_at_diagnosis_city == "Lewiston"),
                                   logan = sum(address_at_diagnosis_city == "Logan"),
                                   mendon = sum(address_at_diagnosis_city == "Mendon"),
                                   millville = sum(address_at_diagnosis_city == "Millville"),
                                   northlogan = sum(address_at_diagnosis_city == "North Logan"),
                                   paradise = sum(address_at_diagnosis_city == "Paradise"),
                                   perry = sum(address_at_diagnosis_city == "Perry"),
                                   providence = sum(address_at_diagnosis_city == "Providence"),
                                   richmond = sum(address_at_diagnosis_city == "Richmond"),
                                   riverheights = sum(address_at_diagnosis_city == "River Heights"),
                                   smithfield = sum(address_at_diagnosis_city == "Smithfield"),
                                   tremonton = sum(address_at_diagnosis_city == "Tremonton"),
                                   wellsville = sum(address_at_diagnosis_city == "Wellsville"),
                                   willard = sum(address_at_diagnosis_city == "Willard")) %>% 
                         mutate(across(where(is.numeric),~replace_na(., replace = 0)),
                                address_at_diagnosis_county = "Bear River")) 

#Incidence Per capita  
BRHD_city <- BRHD_city %>% 
  mutate(brigham_pc = round(brigham*100000/brigham_pop,2),
         garland_pc = round(garland*100000/garland_pop,2),
         hyrum_pc = round(hyrum*100000/hyrum_pop,2),
         lewiston_pc = round(lewiston*100000/lewiston_pop,2),
         logan_pc = round(logan*100000/logan_pop,2),
         mendon_pc = round(mendon*100000/mendon_pop,2),
         millville_pc = round(millville*100000/millville_pop,2),
         northlogan_pc = round(northlogan*100000/northlogan_pop,2),
         paradise_pc = round(paradise*100000/paradise_pop,2),
         perry_pc = round(perry*100000/perry_pop,2),
         providence_pc = round(providence*100000/providence_pop,2),
         richmond_pc = round(richmond*100000/richmond_pop,2),
         riverheights_pc = round(riverheights*100000/riverheights_pop,2),
         smithfield_pc = round(smithfield*100000/smithfield_pop,2),
         tremonton_pc = round(tremonton*100000/tremonton_pop,2),
         wellsville_pc = round(wellsville*100000/wellsville_pop,2),
         willard_pc = round(willard*100000/willard_pop,2))


#Replace NA's with zeros, add 7-day rolling averages
BRHD_city <- BRHD_city %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(brigham_7 = round(SMA(brigham_pc, 7),1)) %>% 
  mutate(garland_7 = round(SMA(garland_pc, 7),1)) %>% 
  mutate(hyrum_7 = round(SMA(hyrum_pc, 7),1)) %>% 
  mutate(lewiston_7 = round(SMA(lewiston_pc, 7),1)) %>% 
  mutate(logan_7 = round(SMA(logan_pc, 7),1)) %>% 
  mutate(mendon_7 = round(SMA(mendon_pc, 7),1)) %>% 
  mutate(millville_7 = round(SMA(millville_pc, 7),1)) %>% 
  mutate(northlogan_7 = round(SMA(northlogan_pc, 7),1)) %>% 
  mutate(paradise_7 = round(SMA(paradise_pc, 7),1)) %>% 
  mutate(perry_7 = round(SMA(perry_pc, 7),1)) %>% 
  mutate(providence_7 = round(SMA(providence_pc, 7),1)) %>% 
  mutate(richmond_7 = round(SMA(richmond_pc, 7),1)) %>% 
  mutate(riverheights_7 = round(SMA(riverheights_pc, 7),1)) %>% 
  mutate(smithfield_7 = round(SMA(smithfield_pc, 7),1)) %>% 
  mutate(tremonton_7 = round(SMA(tremonton_pc, 7),1)) %>% 
  mutate(wellsville_7 = round(SMA(wellsville_pc, 7),1)) %>% 
  mutate(willard_7 = round(SMA(willard_pc, 7),1))

#City Rolling 7-Day Average
BRHD_city <- BRHD_city %>% 
  mutate(brigham_7da = round(SMA(brigham, 7),1)) %>% 
  mutate(garland_7da = round(SMA(garland, 7),1)) %>% 
  mutate(hyrum_7da = round(SMA(hyrum, 7),1)) %>% 
  mutate(lewiston_7da = round(SMA(lewiston, 7),1)) %>% 
  mutate(logan_7da = round(SMA(logan, 7),1)) %>% 
  mutate(mendon_7da = round(SMA(mendon, 7),1)) %>% 
  mutate(millville_7da = round(SMA(millville, 7),1)) %>% 
  mutate(northlogan_7da = round(SMA(northlogan, 7),1)) %>% 
  mutate(paradise_7da = round(SMA(paradise, 7),1)) %>% 
  mutate(perry_7da = round(SMA(perry, 7),1)) %>% 
  mutate(providence_7da = round(SMA(providence, 7),1)) %>% 
  mutate(richmond_7da = round(SMA(richmond, 7),1)) %>% 
  mutate(riverheights_7da = round(SMA(riverheights, 7),1)) %>% 
  mutate(smithfield_7da = round(SMA(smithfield, 7),1)) %>% 
  mutate(tremonton_7da = round(SMA(tremonton, 7),1)) %>% 
  mutate(wellsville_7da = round(SMA(wellsville, 7),1)) %>% 
  mutate(willard_7da = round(SMA(willard, 7),1))


###Test Data Frame
person_pp <- bind_cols(Date = seq.Date(as.Date("2020-03-01"),todays_date,by=1))

person_pp <- left_join(person_pp, export_tests %>%
                          rename(Date = collect_date_fillrph) %>% 
                          group_by(Date) %>% 
                          summarize(BRHD_new_tests = sum(LHD == "Bear River"),
                                    BE_new_tests = sum(county == "Box Elder"),
                                    CA_new_tests = sum(county == "Cache"),
                                    RI_new_tests = sum(county == "Rich"),
                                    new_pos = sum(test_result == "POSITIVE"),
                                    BE_new_pos = sum(county == "Box Elder" & test_result == "POSITIVE"),
                                    CA_new_pos = sum(county == "Cache" & test_result == "POSITIVE"),
                                    RI_new_pos = sum(county == "Rich" & test_result == "POSITIVE"))) %>% 
                      mutate(across(where(is.numeric),~replace_na(., replace = 0)),
                             BRHD_total_tests = cumsum(BRHD_new_tests),
                             Cache_total_tests = cumsum(CA_new_tests),
                             Box_Elder_total_tests = cumsum(BE_new_tests),
                             Rich_total_tests = cumsum(RI_new_tests),
                             BRHD_pp = new_pos/BRHD_new_tests,
                             Cache_pp = CA_new_pos/CA_new_tests,
                             Box_Elder_pp = BE_new_pos/BE_new_tests,
                             Rich_pp = RI_new_pos/RI_new_tests) %>% 
                      mutate(across(where(is.numeric),~replace_na(., replace = 0)),
                             BRHD_7pp = round(100*SMA(BRHD_pp, 7),1),
                             Cache_7pp = round(100*SMA(Cache_pp, 7),1),
                             Box_Elder_7pp = round(100*SMA(Box_Elder_pp, 7),1),
                             Rich_7pp = round(100*SMA(Rich_pp, 7),1),
                             BRHD_7da = round(SMA(BRHD_new_tests, 7),1))

test_pp <- bind_cols(Date = seq.Date(as.Date("2020-03-01"),todays_date,by=1))

test_pp <- left_join(test_pp, export_daytests %>%
                         rename(Date = case_date) %>% 
                         group_by(Date) %>% 
                         summarize(BRHD_new_tests = sum(LHD == "Bear River"),
                                   BE_new_tests = sum(county == "Box Elder"),
                                   CA_new_tests = sum(county == "Cache"),
                                   RI_new_tests = sum(county == "Rich"),
                                   new_pos = sum(test_result == "POSITIVE"),
                                   BE_new_pos = sum(county == "Box Elder" & test_result == "POSITIVE"),
                                   CA_new_pos = sum(county == "Cache" & test_result == "POSITIVE"),
                                   RI_new_pos = sum(county == "Rich" & test_result == "POSITIVE"))) %>% 
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         BRHD_total_tests = cumsum(BRHD_new_tests),
         Cache_total_tests = cumsum(CA_new_tests),
         Box_Elder_total_tests = cumsum(BE_new_tests),
         Rich_total_tests = cumsum(RI_new_tests),
         BRHD_pp = new_pos/BRHD_new_tests,
         Cache_pp = CA_new_pos/CA_new_tests,
         Box_Elder_pp = BE_new_pos/BE_new_tests,
         Rich_pp = RI_new_pos/RI_new_tests) %>% 
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         BRHD_7pp = round(100*SMA(BRHD_pp, 7),1),
         Cache_7pp = round(100*SMA(Cache_pp, 7),1),
         Box_Elder_7pp = round(100*SMA(Box_Elder_pp, 7),1),
         Rich_7pp = round(100*SMA(Rich_pp, 7),1))


###Vaccine
vax_daily <- bind_cols(Date = seq.Date(as.Date("2020-12-17"),todays_date,by=1))

vax_daily <- left_join(vax_daily, cleaned_vax %>%
                          rename(Date = vaccine_date) %>% 
                          group_by(Date) %>% 
                          summarize(#BRHD Doses
                                    BRHD_total = sum(district == "Bear River"),
                                    BRHD_first = sum(district == "Bear River" & dose_all == 1),
                                    BRHD_second = sum(district == "Bear River" & vaccine_status == "Complete"),
                                    BRHD_third = sum(district == "Bear River" & booster_dose == "Yes"),
                                    #BRHD Doses by Race/Ethnicity
                                    BRHD_first_aian = sum(district == "Bear River" & dose_all == 1 & race_eth == "American Indian/Alaska Native"),
                                    BRHD_first_asian = sum(district == "Bear River" & dose_all == 1 & race_eth == "Asian"),
                                    BRHD_first_baa = sum(district == "Bear River" & dose_all == 1 & race_eth == "Black/African American"),
                                    BRHD_first_hisp = sum(district == "Bear River" & dose_all == 1 & race_eth == "Hispanic or Latino"),
                                    BRHD_first_nhpi = sum(district == "Bear River" & dose_all == 1 & race_eth == "Native Hawaiian/Pacific Islander"),
                                    BRHD_first_other = sum(district == "Bear River" & dose_all == 1 & race_eth == "Some other race"),
                                    BRHD_first_urace = sum(district == "Bear River" & dose_all == 1 & race_eth == "Unknown"),
                                    BRHD_first_white = sum(district == "Bear River" & dose_all == 1 & race_eth == "White"),
                                    BRHD_second_aian = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "American Indian/Alaska Native"),
                                    BRHD_second_asian = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "Asian"),
                                    BRHD_second_baa = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "Black/African American"),
                                    BRHD_second_hisp = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "Hispanic or Latino"),
                                    BRHD_second_nhpi = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "Native Hawaiian/Pacific Islander"),
                                    BRHD_second_other = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "Some other race"),
                                    BRHD_second_urace = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "Unknown"),
                                    BRHD_second_white = sum(district == "Bear River" & vaccine_status == "Complete" & race_eth == "White"),
                                    BRHD_third_aian = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "American Indian/Alaska Native"),
                                    BRHD_third_asian = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "Asian"),
                                    BRHD_third_baa = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "Black/African American"),
                                    BRHD_third_hisp = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "Hispanic or Latino"),
                                    BRHD_third_nhpi = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "Native Hawaiian/Pacific Islander"),
                                    BRHD_third_other = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "Some other race"),
                                    BRHD_third_urace = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "Unknown"),
                                    BRHD_third_white = sum(district == "Bear River" & booster_dose == "Yes" & race_eth == "White"),
                                    #BRHD Doses by Age
                                    BRHD_first_5up = sum(district == "Bear River" & dose_all == 1 & age_original >= 5),
                                    BRHD_first_12up = sum(district == "Bear River" & dose_all == 1 & age_original >= 12),
                                    BRHD_first_5_11 = sum(district == "Bear River" & dose_all == 1 & age == "5-11"),
                                    BRHD_first_12_18 = sum(district == "Bear River" & dose_all == 1 & age == "12-18"),
                                    BRHD_first_19_29 = sum(district == "Bear River" & dose_all == 1 & age == "19-29"),
                                    BRHD_first_30_39 = sum(district == "Bear River" & dose_all == 1 & age == "30-39"),
                                    BRHD_first_40_49 = sum(district == "Bear River" & dose_all == 1 & age == "40-49"),
                                    BRHD_first_50_59 = sum(district == "Bear River" & dose_all == 1 & age == "50-59"),
                                    BRHD_first_60_69 = sum(district == "Bear River" & dose_all == 1 & age == "60-69"),
                                    BRHD_first_70_79 = sum(district == "Bear River" & dose_all == 1 & age == "70-79"),
                                    BRHD_first_80up = sum(district == "Bear River" & dose_all == 1 & age == "80+"),
                                    BRHD_first_Unknown = sum(district == "Bear River" & dose_all == 1 & age == "Unknown"),
                                    BRHD_second_5up = sum(district == "Bear River" & vaccine_status == "Complete" & age_original >= 5),
                                    BRHD_second_12up = sum(district == "Bear River" & vaccine_status == "Complete" & age_original >= 12),
                                    BRHD_second_5_11 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "5-11"),
                                    BRHD_second_12_18 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "12-18"),
                                    BRHD_second_19_29 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "19-29"),
                                    BRHD_second_30_39 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "30-39"),
                                    BRHD_second_40_49 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "40-49"),
                                    BRHD_second_50_59 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "50-59"),
                                    BRHD_second_60_69 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "60-69"),
                                    BRHD_second_70_79 = sum(district == "Bear River" & vaccine_status == "Complete" & age == "70-79"),
                                    BRHD_second_80up = sum(district == "Bear River" & vaccine_status == "Complete" & age == "80+"),
                                    BRHD_second_Unknown = sum(district == "Bear River" & vaccine_status == "Complete" & age == "Unknown"),
                                    BRHD_third_12up = sum(district == "Bear River" & booster_dose == "Yes" & age_original >= 12),
                                    BRHD_third_5_11 = sum(district == "Bear River" & booster_dose == "Yes" & age == "5-11"),
                                    BRHD_third_12_18 = sum(district == "Bear River" & booster_dose == "Yes" & age == "12-18"),
                                    BRHD_third_19_29 = sum(district == "Bear River" & booster_dose == "Yes" & age == "19-29"),
                                    BRHD_third_30_39 = sum(district == "Bear River" & booster_dose == "Yes" & age == "30-39"),
                                    BRHD_third_40_49 = sum(district == "Bear River" & booster_dose == "Yes" & age == "40-49"),
                                    BRHD_third_50_59 = sum(district == "Bear River" & booster_dose == "Yes" & age == "50-59"),
                                    BRHD_third_60_69 = sum(district == "Bear River" & booster_dose == "Yes" & age == "60-69"),
                                    BRHD_third_70_79 = sum(district == "Bear River" & booster_dose == "Yes" & age == "70-79"),
                                    BRHD_third_80up = sum(district == "Bear River" & booster_dose == "Yes" & age == "80+"),
                                    BRHD_third_Unknown = sum(district == "Bear River" & booster_dose == "Yes" & age == "Unknown"),
                                    #Cache Doses
                                    Cache_total = sum(county == "Cache"),
                                    Cache_first = sum(county == "Cache" & dose_all == 1),
                                    Cache_second = sum(county == "Cache" & vaccine_status == "Complete"),
                                    Cache_third = sum(county == "Cache" & booster_dose == "Yes"),
                                    #Cache Doses by Age
                                    Cache_first_5up = sum(county == "Cache" & dose_all == 1 & age_original >= 5),
                                    Cache_first_12up = sum(county == "Cache" & dose_all == 1 & age_original >= 12),
                                    Cache_first_5_11 = sum(county == "Cache" & dose_all == 1 & age == "5-11"),
                                    Cache_first_12_18 = sum(county == "Cache" & dose_all == 1 & age == "12-18"),
                                    Cache_first_19_29 = sum(county == "Cache" & dose_all == 1 & age == "19-29"),
                                    Cache_first_30_39 = sum(county == "Cache" & dose_all == 1 & age == "30-39"),
                                    Cache_first_40_49 = sum(county == "Cache" & dose_all == 1 & age == "40-49"),
                                    Cache_first_50_59 = sum(county == "Cache" & dose_all == 1 & age == "50-59"),
                                    Cache_first_60_69 = sum(county == "Cache" & dose_all == 1 & age == "60-69"),
                                    Cache_first_70_79 = sum(county == "Cache" & dose_all == 1 & age == "70-79"),
                                    Cache_first_80up = sum(county == "Cache" & dose_all == 1 & age == "80+"),
                                    Cache_first_Unknown = sum(county == "Cache" & dose_all == 1 & age == "Unknown"),
                                    Cache_second_5up = sum(county == "Cache" & vaccine_status == "Complete" & age_original >= 5),
                                    Cache_second_12up = sum(county == "Cache" & vaccine_status == "Complete" & age_original >= 12),
                                    Cache_second_5_11 = sum(county == "Cache" & vaccine_status == "Complete" & age == "5-11"),
                                    Cache_second_12_18 = sum(county == "Cache" & vaccine_status == "Complete" & age == "12-18"),
                                    Cache_second_19_29 = sum(county == "Cache" & vaccine_status == "Complete" & age == "19-29"),
                                    Cache_second_30_39 = sum(county == "Cache" & vaccine_status == "Complete" & age == "30-39"),
                                    Cache_second_40_49 = sum(county == "Cache" & vaccine_status == "Complete" & age == "40-49"),
                                    Cache_second_50_59 = sum(county == "Cache" & vaccine_status == "Complete" & age == "50-59"),
                                    Cache_second_60_69 = sum(county == "Cache" & vaccine_status == "Complete" & age == "60-69"),
                                    Cache_second_70_79 = sum(county == "Cache" & vaccine_status == "Complete" & age == "70-79"),
                                    Cache_second_80up = sum(county == "Cache" & vaccine_status == "Complete" & age == "80+"),
                                    Cache_second_Unknown = sum(county == "Cache" & vaccine_status == "Complete" & age == "Unknown"),
                                    Cache_third_12up = sum(county == "Cache" & booster_dose == "Yes" & age_original >= 12),
                                    Cache_third_5_11 = sum(county == "Cache" & booster_dose == "Yes" & age == "5-11"),
                                    Cache_third_12_18 = sum(county == "Cache" & booster_dose == "Yes" & age == "12-18"),
                                    Cache_third_19_29 = sum(county == "Cache" & booster_dose == "Yes" & age == "19-29"),
                                    Cache_third_30_39 = sum(county == "Cache" & booster_dose == "Yes" & age == "30-39"),
                                    Cache_third_40_49 = sum(county == "Cache" & booster_dose == "Yes" & age == "40-49"),
                                    Cache_third_50_59 = sum(county == "Cache" & booster_dose == "Yes" & age == "50-59"),
                                    Cache_third_60_69 = sum(county == "Cache" & booster_dose == "Yes" & age == "60-69"),
                                    Cache_third_70_79 = sum(county == "Cache" & booster_dose == "Yes" & age == "70-79"),
                                    Cache_third_80up = sum(county == "Cache" & booster_dose == "Yes" & age == "80+"),
                                    Cache_third_Unknown = sum(county == "Cache" & booster_dose == "Yes" & age == "Unknown"),
                                    #Box Elder Doses
                                    Box_Elder_total = sum(county == "Box Elder"),
                                    Box_Elder_first = sum(county == "Box Elder" & dose_all == 1),
                                    Box_Elder_second = sum(county == "Box Elder" & vaccine_status == "Complete"),
                                    Box_Elder_third = sum(county == "Box Elder" & booster_dose == "Yes"),
                                    #Box Elder Doses by Age
                                    Box_Elder_first_5up = sum(county == "Box Elder" & dose_all == 1 & age_original >= 5),
                                    Box_Elder_first_12up = sum(county == "Box Elder" & dose_all == 1 & age_original >= 12),
                                    Box_Elder_first_5_11 = sum(county == "Box Elder" & dose_all == 1 & age == "5-11"),
                                    Box_Elder_first_12_18 = sum(county == "Box Elder" & dose_all == 1 & age == "12-18"),
                                    Box_Elder_first_19_29 = sum(county == "Box Elder" & dose_all == 1 & age == "19-29"),
                                    Box_Elder_first_30_39 = sum(county == "Box Elder" & dose_all == 1 & age == "30-39"),
                                    Box_Elder_first_40_49 = sum(county == "Box Elder" & dose_all == 1 & age == "40-49"),
                                    Box_Elder_first_50_59 = sum(county == "Box Elder" & dose_all == 1 & age == "50-59"),
                                    Box_Elder_first_60_69 = sum(county == "Box Elder" & dose_all == 1 & age == "60-69"),
                                    Box_Elder_first_70_79 = sum(county == "Box Elder" & dose_all == 1 & age == "70-79"),
                                    Box_Elder_first_80up = sum(county == "Box Elder" & dose_all == 1 & age == "80+"),
                                    Box_Elder_first_Unknown = sum(county == "Box Elder" & dose_all == 1 & age == "Unknown"),
                                    Box_Elder_second_5up = sum(county == "Box Elder" & vaccine_status == "Complete" & age_original >= 5),
                                    Box_Elder_second_12up = sum(county == "Box Elder" & vaccine_status == "Complete" & age_original >= 12),
                                    Box_Elder_second_5_11 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "5-11"),
                                    Box_Elder_second_12_18 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "12-18"),
                                    Box_Elder_second_19_29 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "19-29"),
                                    Box_Elder_second_30_39 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "30-39"),
                                    Box_Elder_second_40_49 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "40-49"),
                                    Box_Elder_second_50_59 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "50-59"),
                                    Box_Elder_second_60_69 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "60-69"),
                                    Box_Elder_second_70_79 = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "70-79"),
                                    Box_Elder_second_80up = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "80+"),
                                    Box_Elder_second_Unknown = sum(county == "Box Elder" & vaccine_status == "Complete" & age == "Unknown"),
                                    Box_Elder_third_12up = sum(county == "Box Elder" & booster_dose == "Yes" & age_original >= 12),
                                    Box_Elder_third_5_11 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "5-11"),
                                    Box_Elder_third_12_18 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "12-18"),
                                    Box_Elder_third_19_29 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "19-29"),
                                    Box_Elder_third_30_39 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "30-39"),
                                    Box_Elder_third_40_49 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "40-49"),
                                    Box_Elder_third_50_59 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "50-59"),
                                    Box_Elder_third_60_69 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "60-69"),
                                    Box_Elder_third_70_79 = sum(county == "Box Elder" & booster_dose == "Yes" & age == "70-79"),
                                    Box_Elder_third_80up = sum(county == "Box Elder" & booster_dose == "Yes" & age == "80+"),
                                    Box_Elder_third_Unknown = sum(county == "Box Elder" & booster_dose == "Yes" & age == "Unknown"),
                                    #Rich Doses
                                    Rich_total = sum(county == "Rich"),
                                    Rich_first = sum(county == "Rich" & dose_all == 1),
                                    Rich_second = sum(county == "Rich" & vaccine_status == "Complete"),
                                    Rich_third = sum(county == "Rich" & booster_dose == "Yes"),
                                    #Rich Doses by Age
                                    Rich_first_5up = sum(county == "Rich" & dose_all == 1 & age_original >= 5),
                                    Rich_first_12up = sum(county == "Rich" & dose_all == 1 & age_original >= 12),
                                    Rich_first_5_11 = sum(county == "Rich" & dose_all == 1 & age == "5-11"),
                                    Rich_first_12_18 = sum(county == "Rich" & dose_all == 1 & age == "12-18"),
                                    Rich_first_19_29 = sum(county == "Rich" & dose_all == 1 & age == "19-29"),
                                    Rich_first_30_39 = sum(county == "Rich" & dose_all == 1 & age == "30-39"),
                                    Rich_first_40_49 = sum(county == "Rich" & dose_all == 1 & age == "40-49"),
                                    Rich_first_50_59 = sum(county == "Rich" & dose_all == 1 & age == "50-59"),
                                    Rich_first_60_69 = sum(county == "Rich" & dose_all == 1 & age == "60-69"),
                                    Rich_first_70_79 = sum(county == "Rich" & dose_all == 1 & age == "70-79"),
                                    Rich_first_80up = sum(county == "Rich" & dose_all == 1 & age == "80+"),
                                    Rich_first_Unknown = sum(county == "Rich" & dose_all == 1 & age == "Unknown"),
                                    Rich_second_5up = sum(county == "Rich" & vaccine_status == "Complete" & age_original >= 5),
                                    Rich_second_12up = sum(county == "Rich" & vaccine_status == "Complete" & age_original >= 12),
                                    Rich_second_5_11 = sum(county == "Rich" & vaccine_status == "Complete" & age == "5-11"),
                                    Rich_second_12_18 = sum(county == "Rich" & vaccine_status == "Complete" & age == "12-18"),
                                    Rich_second_19_29 = sum(county == "Rich" & vaccine_status == "Complete" & age == "19-29"),
                                    Rich_second_30_39 = sum(county == "Rich" & vaccine_status == "Complete" & age == "30-39"),
                                    Rich_second_40_49 = sum(county == "Rich" & vaccine_status == "Complete" & age == "40-49"),
                                    Rich_second_50_59 = sum(county == "Rich" & vaccine_status == "Complete" & age == "50-59"),
                                    Rich_second_60_69 = sum(county == "Rich" & vaccine_status == "Complete" & age == "60-69"),
                                    Rich_second_70_79 = sum(county == "Rich" & vaccine_status == "Complete" & age == "70-79"),
                                    Rich_second_80up = sum(county == "Rich" & vaccine_status == "Complete" & age == "80+"),
                                    Rich_second_Unknown = sum(county == "Rich" & vaccine_status == "Complete" & age == "Unknown"),
                                    Rich_third_12up = sum(county == "Rich" & booster_dose == "Yes" & age_original >= 12),
                                    Rich_third_5_11 = sum(county == "Rich" & booster_dose == "Yes" & age == "5-11"),
                                    Rich_third_12_18 = sum(county == "Rich" & booster_dose == "Yes" & age == "12-18"),
                                    Rich_third_19_29 = sum(county == "Rich" & booster_dose == "Yes" & age == "19-29"),
                                    Rich_third_30_39 = sum(county == "Rich" & booster_dose == "Yes" & age == "30-39"),
                                    Rich_third_40_49 = sum(county == "Rich" & booster_dose == "Yes" & age == "40-49"),
                                    Rich_third_50_59 = sum(county == "Rich" & booster_dose == "Yes" & age == "50-59"),
                                    Rich_third_60_69 = sum(county == "Rich" & booster_dose == "Yes" & age == "60-69"),
                                    Rich_third_70_79 = sum(county == "Rich" & booster_dose == "Yes" & age == "70-79"),
                                    Rich_third_80up = sum(county == "Rich" & booster_dose == "Yes" & age == "80+"),
                                    Rich_third_Unknown = sum(county == "Rich" & booster_dose == "Yes" & age == "Unknown"))) %>% 
  #replace NA's with zeros, create mutated variables from variables created in the step above          
  mutate(across(where(is.numeric),~replace_na(., replace = 0)),
         across(where(is.numeric),~cumsum(.)),
         BRHD_pop_tot = 193330,
         BRHD_pop_aian = 1004,
         BRHD_pop_asian = 2743,
         BRHD_pop_baa = 1217,
         BRHD_pop_hisp = 20710,
         BRHD_pop_nhpi = 762,
         BRHD_pop_other = 525,
         BRHD_pop_white = 161066,
         BRHD_pop_5up = 174678,
         BRHD_pop_12up = 152056,
         BRHD_pop_5_11 = 22622,
         BRHD_pop_12_18 = 23118,
         BRHD_pop_19_29 = 40811,
         BRHD_pop_30_39 = 22841,
         BRHD_pop_40_49 = 20657,
         BRHD_pop_50_59 = 15599,
         BRHD_pop_60_69 = 14979,
         BRHD_pop_70_79 = 9113,
         BRHD_pop_80up = 4938,
         Cache_pop_tot = 130004,
         Cache_pop_5up = 119764,
         Cache_pop_12up = 104472,
         Cache_pop_5_11 = 15292,
         Cache_pop_12_18 = 15824,
         Cache_pop_19_29 = 32527,
         Cache_pop_30_39 = 15104,
         Cache_pop_40_49 = 13511,
         Cache_pop_50_59 = 9720,
         Cache_pop_60_69 = 9040,
         Cache_pop_70_79 = 5700,
         Cache_pop_80up = 3046,
         Box_Elder_pop_tot = 57007,
         Box_Elder_pop_5up = 52630,
         Box_Elder_pop_12up = 45588,
         Box_Elder_pop_5_11 = 7042,
         Box_Elder_pop_12_18 = 7020,
         Box_Elder_pop_19_29 = 8026,
         Box_Elder_pop_30_39 = 7434,
         Box_Elder_pop_40_49 = 6871,
         Box_Elder_pop_50_59 = 5637,
         Box_Elder_pop_60_69 = 5616,
         Box_Elder_pop_70_79 = 3196,
         Box_Elder_pop_80up = 1788,
         Rich_pop_tot = 2452,
         Rich_pop_5up = 2284,
         Rich_pop_12up = 1996,
         Rich_pop_5_11 = 288,
         Rich_pop_12_18 = 274,
         Rich_pop_19_29 = 258,
         Rich_pop_30_39 = 303,
         Rich_pop_40_49 = 275,
         Rich_pop_50_59 = 242,
         Rich_pop_60_69 = 323,
         Rich_pop_70_79 = 217,
         Rich_pop_80up = 104,
         ###Calculate Percentages
         #BRHD
         BRHD_first_per_tot = BRHD_first/BRHD_pop_tot,
         BRHD_second_per_tot = BRHD_second/BRHD_pop_tot,
         BRHD_third_per_tot = BRHD_third/BRHD_pop_tot,
         BRHD_first_per_aian = BRHD_first_aian/BRHD_pop_aian,
         BRHD_first_per_asian = BRHD_first_asian/BRHD_pop_asian,
         BRHD_first_per_baa = BRHD_first_baa/BRHD_pop_baa,
         BRHD_first_per_hisp = BRHD_first_hisp/BRHD_pop_hisp,
         BRHD_first_per_nhpi = BRHD_first_nhpi/BRHD_pop_nhpi,
         BRHD_first_per_other = BRHD_first_other/BRHD_pop_other,
         BRHD_first_per_white = BRHD_first_white/BRHD_pop_white,
         BRHD_second_per_aian = BRHD_second_aian/BRHD_pop_aian,
         BRHD_second_per_asian = BRHD_second_asian/BRHD_pop_asian,
         BRHD_second_per_baa = BRHD_second_baa/BRHD_pop_baa,
         BRHD_second_per_hisp = BRHD_second_hisp/BRHD_pop_hisp,
         BRHD_second_per_nhpi = BRHD_second_nhpi/BRHD_pop_nhpi,
         BRHD_second_per_other = BRHD_second_other/BRHD_pop_other,
         BRHD_second_per_white = BRHD_second_white/BRHD_pop_white,
         BRHD_third_per_aian = BRHD_third_aian/BRHD_pop_aian,
         BRHD_third_per_asian = BRHD_third_asian/BRHD_pop_asian,
         BRHD_third_per_baa = BRHD_third_baa/BRHD_pop_baa,
         BRHD_third_per_hisp = BRHD_third_hisp/BRHD_pop_hisp,
         BRHD_third_per_nhpi = BRHD_third_nhpi/BRHD_pop_nhpi,
         BRHD_third_per_other = BRHD_third_other/BRHD_pop_other,
         BRHD_third_per_white = BRHD_third_white/BRHD_pop_white,
         BRHD_first_per_5up = BRHD_first_5up/BRHD_pop_5up,
         BRHD_first_per_12up = BRHD_first_12up/BRHD_pop_12up,
         BRHD_first_per_5_11 = BRHD_first_5_11/BRHD_pop_5_11,
         BRHD_first_per_12_18 = BRHD_first_12_18/BRHD_pop_12_18,
         BRHD_first_per_19_29 = BRHD_first_19_29/BRHD_pop_19_29,
         BRHD_first_per_30_39 = BRHD_first_30_39/BRHD_pop_30_39,
         BRHD_first_per_40_49 = BRHD_first_40_49/BRHD_pop_40_49,
         BRHD_first_per_50_59 = BRHD_first_50_59/BRHD_pop_50_59,
         BRHD_first_per_60_69 = BRHD_first_60_69/BRHD_pop_60_69,
         BRHD_first_per_70_79 = BRHD_first_70_79/BRHD_pop_70_79,
         BRHD_first_per_80up = BRHD_first_80up/BRHD_pop_80up,
         BRHD_second_per_5up = BRHD_second_5up/BRHD_pop_5up,
         BRHD_second_per_12up = BRHD_second_12up/BRHD_pop_12up,
         BRHD_second_per_5_11 = BRHD_second_5_11/BRHD_pop_5_11,
         BRHD_second_per_12_18 = BRHD_second_12_18/BRHD_pop_12_18,
         BRHD_second_per_19_29 = BRHD_second_19_29/BRHD_pop_19_29,
         BRHD_second_per_30_39 = BRHD_second_30_39/BRHD_pop_30_39,
         BRHD_second_per_40_49 = BRHD_second_40_49/BRHD_pop_40_49,
         BRHD_second_per_50_59 = BRHD_second_50_59/BRHD_pop_50_59,
         BRHD_second_per_60_69 = BRHD_second_60_69/BRHD_pop_60_69,
         BRHD_second_per_70_79 = BRHD_second_70_79/BRHD_pop_70_79,
         BRHD_second_per_80up = BRHD_second_80up/BRHD_pop_80up,
         BRHD_third_per_12up = BRHD_third_12up/BRHD_pop_12up,
         BRHD_third_per_12_18 = BRHD_third_12_18/BRHD_pop_12_18,
         BRHD_third_per_19_29 = BRHD_third_19_29/BRHD_pop_19_29,
         BRHD_third_per_30_39 = BRHD_third_30_39/BRHD_pop_30_39,
         BRHD_third_per_40_49 = BRHD_third_40_49/BRHD_pop_40_49,
         BRHD_third_per_50_59 = BRHD_third_50_59/BRHD_pop_50_59,
         BRHD_third_per_60_69 = BRHD_third_60_69/BRHD_pop_60_69,
         BRHD_third_per_70_79 = BRHD_third_70_79/BRHD_pop_70_79,
         BRHD_third_per_80up = BRHD_third_80up/BRHD_pop_80up,
         #Cache
         Cache_first_per_tot = Cache_first/Cache_pop_tot,
         Cache_second_per_tot = Cache_second/Cache_pop_tot,
         Cache_third_per_tot = Cache_third/Cache_pop_tot,
         Cache_first_per_5up = Cache_first_5up/Cache_pop_5up,
         Cache_first_per_12up = Cache_first_12up/Cache_pop_12up,
         Cache_first_per_5_11 = Cache_first_5_11/Cache_pop_5_11,
         Cache_first_per_12_18 = Cache_first_12_18/Cache_pop_12_18,
         Cache_first_per_19_29 = Cache_first_19_29/Cache_pop_19_29,
         Cache_first_per_30_39 = Cache_first_30_39/Cache_pop_30_39,
         Cache_first_per_40_49 = Cache_first_40_49/Cache_pop_40_49,
         Cache_first_per_50_59 = Cache_first_50_59/Cache_pop_50_59,
         Cache_first_per_60_69 = Cache_first_60_69/Cache_pop_60_69,
         Cache_first_per_70_79 = Cache_first_70_79/Cache_pop_70_79,
         Cache_first_per_80up = Cache_first_80up/Cache_pop_80up,
         Cache_second_per_5up = Cache_second_5up/Cache_pop_5up,
         Cache_second_per_12up = Cache_second_12up/Cache_pop_12up,
         Cache_second_per_5_11 = Cache_second_5_11/Cache_pop_5_11,
         Cache_second_per_12_18 = Cache_second_12_18/Cache_pop_12_18,
         Cache_second_per_19_29 = Cache_second_19_29/Cache_pop_19_29,
         Cache_second_per_30_39 = Cache_second_30_39/Cache_pop_30_39,
         Cache_second_per_40_49 = Cache_second_40_49/Cache_pop_40_49,
         Cache_second_per_50_59 = Cache_second_50_59/Cache_pop_50_59,
         Cache_second_per_60_69 = Cache_second_60_69/Cache_pop_60_69,
         Cache_second_per_70_79 = Cache_second_70_79/Cache_pop_70_79,
         Cache_second_per_80up = Cache_second_80up/Cache_pop_80up,
         Cache_third_per_12up = Cache_third_12up/Cache_pop_12up,
         Cache_third_per_5_11 = Cache_third_5_11/Cache_pop_5_11,
         Cache_third_per_12_18 = Cache_third_12_18/Cache_pop_12_18,
         Cache_third_per_19_29 = Cache_third_19_29/Cache_pop_19_29,
         Cache_third_per_30_39 = Cache_third_30_39/Cache_pop_30_39,
         Cache_third_per_40_49 = Cache_third_40_49/Cache_pop_40_49,
         Cache_third_per_50_59 = Cache_third_50_59/Cache_pop_50_59,
         Cache_third_per_60_69 = Cache_third_60_69/Cache_pop_60_69,
         Cache_third_per_70_79 = Cache_third_70_79/Cache_pop_70_79,
         Cache_third_per_80up = Cache_third_80up/Cache_pop_80up,
         #Box Elder
         Box_Elder_first_per_tot = Box_Elder_first/Box_Elder_pop_tot,
         Box_Elder_second_per_tot = Box_Elder_second/Box_Elder_pop_tot,
         Box_Elder_third_per_tot = Box_Elder_third/Box_Elder_pop_tot,
         Box_Elder_first_per_5up = Box_Elder_first_5up/Box_Elder_pop_5up,
         Box_Elder_first_per_12up = Box_Elder_first_12up/Box_Elder_pop_12up,
         Box_Elder_first_per_5_11 = Box_Elder_first_5_11/Box_Elder_pop_5_11,
         Box_Elder_first_per_12_18 = Box_Elder_first_12_18/Box_Elder_pop_12_18,
         Box_Elder_first_per_19_29 = Box_Elder_first_19_29/Box_Elder_pop_19_29,
         Box_Elder_first_per_30_39 = Box_Elder_first_30_39/Box_Elder_pop_30_39,
         Box_Elder_first_per_40_49 = Box_Elder_first_40_49/Box_Elder_pop_40_49,
         Box_Elder_first_per_50_59 = Box_Elder_first_50_59/Box_Elder_pop_50_59,
         Box_Elder_first_per_60_69 = Box_Elder_first_60_69/Box_Elder_pop_60_69,
         Box_Elder_first_per_70_79 = Box_Elder_first_70_79/Box_Elder_pop_70_79,
         Box_Elder_first_per_80up = Box_Elder_first_80up/Box_Elder_pop_80up,
         Box_Elder_second_per_5up = Box_Elder_second_5up/Box_Elder_pop_5up,
         Box_Elder_second_per_12up = Box_Elder_second_12up/Box_Elder_pop_12up,
         Box_Elder_second_per_5_11 = Box_Elder_second_5_11/Box_Elder_pop_5_11,
         Box_Elder_second_per_12_18 = Box_Elder_second_12_18/Box_Elder_pop_12_18,
         Box_Elder_second_per_19_29 = Box_Elder_second_19_29/Box_Elder_pop_19_29,
         Box_Elder_second_per_30_39 = Box_Elder_second_30_39/Box_Elder_pop_30_39,
         Box_Elder_second_per_40_49 = Box_Elder_second_40_49/Box_Elder_pop_40_49,
         Box_Elder_second_per_50_59 = Box_Elder_second_50_59/Box_Elder_pop_50_59,
         Box_Elder_second_per_60_69 = Box_Elder_second_60_69/Box_Elder_pop_60_69,
         Box_Elder_second_per_70_79 = Box_Elder_second_70_79/Box_Elder_pop_70_79,
         Box_Elder_second_per_80up = Box_Elder_second_80up/Box_Elder_pop_80up,
         Box_Elder_third_per_12up = Box_Elder_third_12up/Box_Elder_pop_12up,
         Box_Elder_third_per_5_11 = Box_Elder_third_5_11/Box_Elder_pop_5_11,
         Box_Elder_third_per_12_18 = Box_Elder_third_12_18/Box_Elder_pop_12_18,
         Box_Elder_third_per_19_29 = Box_Elder_third_19_29/Box_Elder_pop_19_29,
         Box_Elder_third_per_30_39 = Box_Elder_third_30_39/Box_Elder_pop_30_39,
         Box_Elder_third_per_40_49 = Box_Elder_third_40_49/Box_Elder_pop_40_49,
         Box_Elder_third_per_50_59 = Box_Elder_third_50_59/Box_Elder_pop_50_59,
         Box_Elder_third_per_60_69 = Box_Elder_third_60_69/Box_Elder_pop_60_69,
         Box_Elder_third_per_70_79 = Box_Elder_third_70_79/Box_Elder_pop_70_79,
         Box_Elder_third_per_80up = Box_Elder_third_80up/Box_Elder_pop_80up,
         #Rich
         Rich_first_per_tot = Rich_first/Rich_pop_tot,
         Rich_second_per_tot = Rich_second/Rich_pop_tot,
         Rich_third_per_tot = Rich_third/Rich_pop_tot,
         Rich_first_per_5up = Rich_first_5up/Rich_pop_5up,
         Rich_first_per_12up = Rich_first_12up/Rich_pop_12up,
         Rich_first_per_5_11 = Rich_first_5_11/Rich_pop_5_11,
         Rich_first_per_12_18 = Rich_first_12_18/Rich_pop_12_18,
         Rich_first_per_19_29 = Rich_first_19_29/Rich_pop_19_29,
         Rich_first_per_30_39 = Rich_first_30_39/Rich_pop_30_39,
         Rich_first_per_40_49 = Rich_first_40_49/Rich_pop_40_49,
         Rich_first_per_50_59 = Rich_first_50_59/Rich_pop_50_59,
         Rich_first_per_60_69 = Rich_first_60_69/Rich_pop_60_69,
         Rich_first_per_70_79 = Rich_first_70_79/Rich_pop_70_79,
         Rich_first_per_80up = Rich_first_80up/Rich_pop_80up,
         Rich_second_per_5up = Rich_second_5up/Rich_pop_5up,
         Rich_second_per_12up = Rich_second_12up/Rich_pop_12up,
         Rich_second_per_5_11 = Rich_second_5_11/Rich_pop_5_11,
         Rich_second_per_12_18 = Rich_second_12_18/Rich_pop_12_18,
         Rich_second_per_19_29 = Rich_second_19_29/Rich_pop_19_29,
         Rich_second_per_30_39 = Rich_second_30_39/Rich_pop_30_39,
         Rich_second_per_40_49 = Rich_second_40_49/Rich_pop_40_49,
         Rich_second_per_50_59 = Rich_second_50_59/Rich_pop_50_59,
         Rich_second_per_60_69 = Rich_second_60_69/Rich_pop_60_69,
         Rich_second_per_70_79 = Rich_second_70_79/Rich_pop_70_79,
         Rich_second_per_80up = Rich_second_80up/Rich_pop_80up,
         Rich_third_per_12up = Rich_third_12up/Rich_pop_12up,
         Rich_third_per_5_11 = Rich_third_5_11/Rich_pop_5_11,
         Rich_third_per_12_18 = Rich_third_12_18/Rich_pop_12_18,
         Rich_third_per_19_29 = Rich_third_19_29/Rich_pop_19_29,
         Rich_third_per_30_39 = Rich_third_30_39/Rich_pop_30_39,
         Rich_third_per_40_49 = Rich_third_40_49/Rich_pop_40_49,
         Rich_third_per_50_59 = Rich_third_50_59/Rich_pop_50_59,
         Rich_third_per_60_69 = Rich_third_60_69/Rich_pop_60_69,
         Rich_third_per_70_79 = Rich_third_70_79/Rich_pop_70_79,
         Rich_third_per_80up = Rich_third_80up/Rich_pop_80up) %>% 
  mutate_at(vars(contains("per")), ~ .x*100) %>% 
  mutate_at(vars(contains("per")), ~round(.,2))

         
#Population Estimates from IBIS, by race from link below
#https://data.census.gov/cedsci/table?g=0500000US49003,49005,49033&y=2020&d=DEC%20Redistricting%20Data%20%28PL%2094-171%29&tid=DECENNIALPL2020.P2


###Total Table English
total <- bind_cols(Region = c("Box Elder","Cache","Rich","Bear River"),
                   `Total Deaths`= c(nrow(export_deaths[export_deaths$county_name == "Box Elder",]),
                                     nrow(export_deaths[export_deaths$county_name == "Cache",]),
                                     nrow(export_deaths[export_deaths$county_name == "Rich",]),
                                     nrow(export_deaths[export_deaths$lhd == 1,])),
                   `Total Hospitalizations`= c(nrow(hosp[hosp$county_name == "Box Elder",]),
                                     nrow(hosp[hosp$county_name == "Cache",]),
                                     nrow(hosp[hosp$county_name == "Rich",]),
                                     nrow(hosp[hosp$lhd == 1,])),
                   `Total Cases` = c(nrow(export_cases[export_cases$county_name == "Box Elder",]),
                                      nrow(export_cases[export_cases$county_name == "Cache",]),
                                      nrow(export_cases[export_cases$county_name == "Rich",]),
                                      nrow(export_cases[export_cases$lhd == 1,])),
                   `New Weekly Cases` = c(as.integer(last(case_week$Box_Elder_new_week)),
                                   as.integer(last(case_week$Cache_new_week)),
                                   as.integer(last(case_week$Rich_new_week)),
                                   as.integer(last(case_week$BRHD_new_week))),
                   `7-Day Average` = c(last(case_daily$Box_Elder_7d),
                                       last(case_daily$Cache_7d),
                                       last(case_daily$Rich_7d),
                                       last(case_daily$BRHD_7d)),
                  `7-Day Positivity` = c(last(test_pp$Box_Elder_7pp),
                                      last(test_pp$Cache_7pp),
                                      last(test_pp$Rich_7pp),
                                      last(test_pp$BRHD_7pp)))

###Total Table Spanish
total_sp <- bind_cols(Region = c("Box Elder","Cache","Rich","Bear River"),
                   `Muertes Totales`= c(nrow(export_deaths[export_deaths$county_name == "Box Elder",]),
                                     nrow(export_deaths[export_deaths$county_name == "Cache",]),
                                     nrow(export_deaths[export_deaths$county_name == "Rich",]),
                                     nrow(export_deaths[export_deaths$lhd == 1,])),
                   `Hospitalizados Totales`= c(nrow(hosp[hosp$county_name == "Box Elder",]),
                                               nrow(hosp[hosp$county_name == "Cache",]),
                                               nrow(hosp[hosp$county_name == "Rich",]),
                                               nrow(hosp[hosp$lhd == 1,])),
                   `Casos Totales` = c(nrow(export_cases[export_cases$county_name == "Box Elder",]),
                                     nrow(export_cases[export_cases$county_name == "Cache",]),
                                     nrow(export_cases[export_cases$county_name == "Rich",]),
                                     nrow(export_cases[export_cases$lhd == 1,])),
                   `Casos Nuevos Semanales` = c(as.integer(last(case_week$Box_Elder_new_week)),
                                      as.integer(last(case_week$Cache_new_week)),
                                      as.integer(last(case_week$Rich_new_week)),
                                      as.integer(last(case_week$BRHD_new_week))),
                   `Promedia de 7-días` = c(last(case_daily$Box_Elder_7d),
                                       last(case_daily$Cache_7d),
                                       last(case_daily$Rich_7d),
                                       last(case_daily$BRHD_7d)),
                   `Positividad de 7-días` = c(last(test_pp$Box_Elder_7pp),
                                          last(test_pp$Cache_7pp),
                                          last(test_pp$Rich_7pp),
                                          last(test_pp$BRHD_7pp)))
###Total Table Vax English
total_vax <- bind_cols(Region = c("Box Elder","Cache","Rich","Bear River"),
                   `Total Doses`= c(as.integer(last(vax_daily$Box_Elder_total)),
                                    as.integer(last(vax_daily$Cache_total)),
                                    as.integer(last(vax_daily$Rich_total)),
                                    as.integer(last(vax_daily$BRHD_total))),
                   `Percent of Population (Ages 5+) Fully Vaccinated ` = c(as.integer(last(vax_daily$Box_Elder_second_per_5up)),
                                                                                as.integer(last(vax_daily$Cache_second_per_5up)),
                                                                                as.integer(last(vax_daily$Rich_second_per_5up)),
                                                                                as.integer(last(vax_daily$BRHD_second_per_5up))),
                   `Percent of Population (Ages 5+) with at Least One Dose` = c(as.integer(last(vax_daily$Box_Elder_first_per_5up)),
                                                                                as.integer(last(vax_daily$Cache_first_per_5up)),
                                                                                as.integer(last(vax_daily$Rich_first_per_5up)),
                                                                                as.integer(last(vax_daily$BRHD_first_per_5up))))
                   #`Number of People (Ages 5+) Fully Vaccinated` = c(as.integer(last(vax_daily$Box_Elder_second_5up)),
                                                                    # as.integer(last(vax_daily$Cache_second_5up)),
                                                                     #as.integer(last(vax_daily$Rich_second_5up)),
                                                                     #as.integer(last(vax_daily$BRHD_second_5up))))
###Total Table Vax Spanish
total_vaxsp <- bind_cols(Region = c("Box Elder","Cache","Rich","Bear River"),
                       `Dosis Totales`= c(as.integer(last(vax_daily$Box_Elder_total)),
                                        as.integer(last(vax_daily$Cache_total)),
                                        as.integer(last(vax_daily$Rich_total)),
                                        as.integer(last(vax_daily$BRHD_total))),
                       `Porcentaje de la población elegible para la vacuna (mayores de 5 años) con serie de vacunas completa` = c(as.integer(last(vax_daily$Box_Elder_second_per_5up)),
                                                                               as.integer(last(vax_daily$Cache_second_per_5up)),
                                                                               as.integer(last(vax_daily$Rich_second_per_5up)),
                                                                               as.integer(last(vax_daily$BRHD_second_per_5up))),
                       `Porcentaje de la población elegible para la vacuna (mayores de 5 años) con al menos una dosis` = c(as.integer(last(vax_daily$Box_Elder_first_per_5up)),
                                                                                    as.integer(last(vax_daily$Cache_first_per_5up)),
                                                                                    as.integer(last(vax_daily$Rich_first_per_5up)),
                                                                                    as.integer(last(vax_daily$BRHD_first_per_5up))))

                     
#####################
# Charts in English #
#####################


###Death Charts
weekly_deaths <- plot_ly(death_week, x = ~week_of, y = ~weekly_deaths, name = 'Weekly Deaths', type = 'bar')
weekly_deaths <- weekly_deaths %>% add_trace(y = ~deaths_4wa, name = '4 Week Average', type = 'scatter', mode = 'lines', fill = 'tozeroy')
weekly_deaths <- weekly_deaths %>% layout(xaxis = list(title = "Date"),
                                          yaxis = list (title = "Deaths"),
                                          shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                             line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))


#Vaccination Status
vax_death <- plot_ly(death_week, x = ~week_of, y = ~vaccinated, type = 'bar', name = 'Vaccinated (2+ doses)')
vax_death <- vax_death %>% add_trace(y = ~unvaccinated, name = 'Unvaccinated')
vax_death <- vax_death %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Deaths'), barmode = 'stack',
                                  shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                     line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))

#County
county_death <- plot_ly(death_week, x = ~week_of, y = ~`Box Elder`, type = 'bar', name = 'Box Elder')
county_death <- county_death %>% add_trace(y = ~Cache, name = 'Cache')
county_death <- county_death %>% add_trace(y = ~Rich, name = 'Rich')
county_death <- county_death %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Deaths'), barmode = 'stack',
                                        shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                           line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))

#Age
age_death <- plot_ly(death_week, x = ~week_of, y = ~`85+`, type = 'bar', name = '85+')
age_death <- age_death %>% add_trace(y = ~`65-84`, name = '65-84')
age_death <- age_death %>% add_trace(y = ~`45-64`, name = '45-64')
age_death <- age_death %>% add_trace(y = ~`25-44`, name = '25-44')
age_death <- age_death %>% add_trace(y = ~`15-24`, name = '15-24')
age_death <- age_death %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Deaths'), barmode = 'stack',
                                  shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                     line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))

vax_death_month <- plot_ly(death_month, x = ~Month, y = ~vaccinated, type = 'bar', name = 'Vaccinated (2+ doses)')
vax_death_month <- vax_death_month %>% add_trace(y = ~unvaccinated, name = 'Unvaccinated')
vax_death_month <- vax_death_month %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Deaths'), barmode = 'stack',
                                              shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                                 line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = 32, x0 = todays_date - 14, x1 = todays_date)))
###Hospitalization Charts

hosp_df <- hosp_week %>%
  dplyr::select(week_of, weekly_hosp, hosp_4wa)

weekly_hosp <- plot_ly(hosp_df, x = ~week_of, y = ~weekly_hosp, name = 'Weekly Hospitalizations', type = 'bar')
weekly_hosp <- weekly_hosp %>% add_trace(y = ~hosp_4wa, name = '4 Week Average', type = 'scatter', mode = 'lines', fill = 'tozeroy')
weekly_hosp <- weekly_hosp %>% layout(xaxis = list(title = "Date"),
                                      yaxis = list (title = "Hospitalizations"),
                                      shapes = list(list(type = "rect", text = "Hospitalizations in this time may have not yet been reported", fillcolor = 'grey',
                                                         line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))


#Vaccination Status
vax_hosp <- plot_ly(hosp_week, x = ~week_of, y = ~vaccinated, type = 'bar', name = 'Vaccinated (2+ doses)')
vax_hosp <- vax_hosp %>% add_trace(y = ~unvaccinated, name = 'Unvaccinated')
vax_hosp <- vax_hosp %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Hospitalizations'), barmode = 'stack',
                                shapes = list(list(type = "rect", text = "Hospitalizations in this time may have not yet been reported", fillcolor = 'grey',
                                                   line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))

#County
county_hosp <- plot_ly(hosp_week, x = ~week_of, y = ~`Box Elder`, type = 'bar', name = 'Box Elder')
county_hosp <- county_hosp %>% add_trace(y = ~Cache, name = 'Cache')
county_hosp <- county_hosp %>% add_trace(y = ~Rich, name = 'Rich')
county_hosp <- county_hosp %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Hospitalizations'), barmode = 'stack',
                                      shapes = list(list(type = "rect", text = "Hospitalizations in this time may have not yet been reported", fillcolor = 'grey',
                                                         line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))

#Age
age_hosp <- plot_ly(hosp_week, x = ~week_of, y = ~`85+`, type = 'bar', name = '85+')
age_hosp <- age_hosp %>% add_trace(y = ~`65-84`, name = '65-84')
age_hosp <- age_hosp %>% add_trace(y = ~`45-64`, name = '45-64')
age_hosp <- age_hosp %>% add_trace(y = ~`25-44`, name = '25-44')
age_hosp <- age_hosp %>% add_trace(y = ~`15-24`, name = '15-24')
age_hosp <- age_hosp %>% add_trace(y = ~`1-14`, name = '1-14')
age_hosp <- age_hosp %>% add_trace(y = ~`<1`, name = '<1')
age_hosp <- age_hosp %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Hospitalizations'), barmode = 'stack',
                                shapes = list(list(type = "rect", text = "Hospitalizations in this time may have not yet been reported", fillcolor = 'grey',
                                                   line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))


###Case Charts

weekly_case <- plot_ly(case_week, x = ~week_of, y = ~BRHD_new_week, type = 'bar', name = 'New Weekly Cases')
weekly_case <- weekly_case %>% add_trace(y = ~BRHD_4wa, name = '4 Week Average', type = 'scatter', mode = 'lines', fill = 'tozeroy')
weekly_case <- weekly_case %>% layout(xaxis = list(title = "Date"),
                                      yaxis = list (title = "New Weekly Cases"))
weekly_case

county_percap <- plot_ly(case_daily, x = ~Date, y = ~BRHD_pc7, type = 'scatter', mode = "lines", name = 'BRHD 7-Day Average per Capita')
county_percap <- county_percap %>% add_trace(y = ~Box_Elder_pc7, name = 'Box Elder 7-Day Average per Capita', mode = 'lines')
county_percap <- county_percap %>% add_trace(y = ~Cache_pc7, name = 'Cache 7-Day Average per Capita', mode = 'lines')
county_percap <- county_percap %>% add_trace(y = ~Rich_pc7, name = 'Rich 7-Day Average per Capita', mode = 'lines')
county_percap <- county_percap %>% layout(xaxis = list(title = "Date"),
                                    yaxis = list (title = "New Cases"))


county_rate <- plot_ly(case_daily, x = ~Date, y = ~BRHD_7d, type = 'scatter', mode = "lines", name = 'BRHD 7-Day Average')
county_rate <- county_rate %>% add_trace(y = ~Box_Elder_7d, name = 'Box Elder 7-Day Average', mode = 'lines')
county_rate <- county_rate %>% add_trace(y = ~Cache_7d, name = 'Cache 7-Day Average', mode = 'lines')
county_rate <- county_rate %>% add_trace(y = ~Rich_7d, name = 'Rich 7-Day Average', mode = 'lines')
county_rate <- county_rate %>% layout(xaxis = list(title = "Date"),
                                      yaxis = list (title = "New Cases"))

county_srate <- plot_ly(case_daily, x = ~Date, y = ~BRHD_pcrate, type = 'scatter', mode = "lines", name = 'BRHD 7-Day Average per Capita')
county_srate <- county_srate %>% add_trace(y = ~Box_Elder_pcrate, name = 'Box Elder 7-Day Rate', mode = 'lines')
county_srate <- county_srate %>% add_trace(y = ~Cache_pcrate, name = 'Cache 7-Day Rate', mode = 'lines')
county_srate <- county_srate %>% add_trace(y = ~Rich_pcrate, name = 'Rich 7-Day Rate', mode = 'lines')
county_srate <- county_srate %>% layout(xaxis = list(title = "Date"),
                                          yaxis = list (title = "Case Rate"))

#7-day average by city crude numbers
city_sda <- plot_ly(BRHD_city, x = ~Date, y = ~brigham_7da, name = 'Brigham City', type = 'scatter', mode = 'lines',
                    line = list(color = '#800000', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~garland_7da, name = 'Garland', line = list(color = '#000000', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~hyrum_7da, name = 'Hyrum', line = list(color = '#8B4513', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~lewiston_7da, name = 'Lewiston', line = list(color = '#E9967A', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~logan_7da, name = 'Logan', line = list(color = '#BC8F8F', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~mendon_7da, name = 'Mendon', line = list(color = '#FF1493', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~millville_7da, name = 'Millville', line = list(color = '#8B008B', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~northlogan_7da, name = 'North Logan', line = list(color = '#8A2BE2', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~paradise_7da, name = 'Paradise', line = list(color = '#00008B', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~perry_7da, name = 'Perry', line = list(color = '#008B8B', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~providence_7da, name = 'Providence', line = list(color = '#20B2AA', width = 2)) 
city_sda <- city_sda %>% add_trace(y = ~richmond_7da, name = 'Richmond', line = list(color = '#00FF7F', width = 2))
city_sda <- city_sda %>% add_trace(y = ~riverheights_7da, name = 'River Heights', line = list(color = '#008000', width = 2))
city_sda <- city_sda %>% add_trace(y = ~smithfield_7da, name = 'Smithfield', line = list(color = '#9ACD32', width = 2))
city_sda <- city_sda %>% add_trace(y = ~tremonton_7da, name = 'Tremonton', line = list(color = '#FFD700', width = 2))
city_sda <- city_sda %>% add_trace(y = ~wellsville_7da, name = 'Wellsville', line = list(color = '#FF8C00', width = 2))
city_sda <- city_sda %>% add_trace(y = ~willard_7da, name = 'Willard', line = list(color = '#FF6347', width = 2))
city_sda <- city_sda %>% layout(xaxis = list(title = "Date"),
                                yaxis = list (title = "Daily Cases"))

vax_case <- plot_ly(case_week, x = ~week_of, y = ~vaccinated, type = 'bar', name = 'Vaccinated (2+ doses)')
vax_case <- vax_case %>% add_trace(y = ~unvaccinated, name = 'Unvaccinated')
vax_case <- vax_case %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Weekly Cases'), barmode = 'stack')


reinf_case <- plot_ly(case_week, x = ~week_of, y = ~reinfection, type = 'bar', name = 'Reinfection')
reinf_case <- reinf_case %>% add_trace(y = ~firstinfection, name = 'First Infection')
reinf_case <- reinf_case %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Weekly Cases'), barmode = 'stack')

age_case <- plot_ly(case_week, x = ~week_of, y = ~`85+`, type = 'bar', name = '85+')
age_case <- age_case %>% add_trace(y = ~`65-84`, name = '65-84')
age_case <- age_case %>% add_trace(y = ~`45-64`, name = '45-64')
age_case <- age_case %>% add_trace(y = ~`25-44`, name = '25-44')
age_case <- age_case %>% add_trace(y = ~`15-24`, name = '15-24')
age_case <- age_case %>% add_trace(y = ~`1-14`, name = '1-14')
age_case <- age_case %>% add_trace(y = ~`<1`, name = '<1')
age_case <- age_case %>% layout(xaxis = list(title = "Date"), yaxis = list(title = 'Weekly Cases'), barmode = 'stack')

#Tests
person_tests <- plot_ly(person_pp, x = ~Date, y = ~BRHD_7pp, type = 'scatter', mode = "lines", name = 'BRHD 7-Day Percent Positivity')
person_tests <- person_tests %>% add_trace(y = ~Box_Elder_7pp, name = 'Box Elder 7-Day Percent Positivity', mode = 'lines')
person_tests <- person_tests %>% add_trace(y = ~Cache_7pp, name = 'Cache 7-Day Percent Positivity', mode = 'lines')
person_tests <- person_tests %>% layout(xaxis = list(title = "Date"),
                                      yaxis = list (title = "Percent Positivity"))

test_tests <- plot_ly(test_pp, x = ~Date, y = ~BRHD_7pp, type = 'scatter', mode = "lines", name = 'BRHD 7-Day Percent Positivity')
test_tests <- test_tests %>% add_trace(y = ~Box_Elder_7pp, name = 'Box Elder 7-Day Percent Positivity', mode = 'lines')
test_tests <- test_tests %>% add_trace(y = ~Cache_7pp, name = 'Cache 7-Day Percent Positivity', mode = 'lines')
test_tests <- test_tests %>% layout(xaxis = list(title = "Date"),
                                        yaxis = list (title = "Percent Positivity"))

daily_tests <- plot_ly(person_pp, x = ~Date, y = ~BRHD_new_tests, type = 'bar', name = 'New Tests')
daily_tests <- daily_tests %>% add_trace(y = ~BRHD_7da, name = '7-Day Average', type = 'scatter', mode = 'lines', fill = 'tozeroy')
daily_tests <- daily_tests %>% layout(xaxis = list(title = "Date"),
                                      yaxis = list (title = "New Tests"))
###Vaccine Charts
vax_chart <- plot_ly(vax_daily, x = ~Date, y = ~BRHD_first_per_5up, type = 'scatter', mode = "lines", name = 'BRHD')
vax_chart <- vax_chart %>% add_trace(y = ~Cache_first_per_5up, name = 'Cache', mode = 'lines')
vax_chart <- vax_chart %>% add_trace(y = ~Box_Elder_first_per_5up, name = 'Box Elder', mode = 'lines')
vax_chart <- vax_chart %>% add_trace(y = ~Rich_first_per_5up, name = 'Rich', mode = 'lines')
vax_chart <- vax_chart %>% layout(xaxis = list(title = "Date"),
                                  yaxis = list (title = "Percent"))

vax_chart1 <- plot_ly(vax_daily, x = ~Date, y = ~BRHD_second_per_5up, type = 'scatter', mode = "lines", name = 'BRHD')
vax_chart1 <- vax_chart1 %>% add_trace(y = ~Cache_second_per_5up, name = 'Cache', mode = 'lines')
vax_chart1 <- vax_chart1 %>% add_trace(y = ~Box_Elder_second_per_5up, name = 'Box Elder', mode = 'lines')
vax_chart1 <- vax_chart1 %>% add_trace(y = ~Rich_second_per_5up, name = 'Rich', mode = 'lines')
vax_chart1 <- vax_chart1 %>% layout(xaxis = list(title = "Date"),
                                  yaxis = list (title = "Percent"))

vax_chart2 <- plot_ly(vax_daily, x = ~Date, y = ~BRHD_third_per_12up, type = 'scatter', mode = "lines", name = 'BRHD')
vax_chart2 <- vax_chart2 %>% add_trace(y = ~Cache_third_per_12up, name = 'Cache', mode = 'lines')
vax_chart2 <- vax_chart2 %>% add_trace(y = ~Box_Elder_third_per_12up, name = 'Box Elder', mode = 'lines')
vax_chart2 <- vax_chart2 %>% add_trace(y = ~Rich_third_per_12up, name = 'Rich', mode = 'lines')
vax_chart2 <- vax_chart2 %>% layout(xaxis = list(title = "Date"),
                                    yaxis = list (title = "Percent"))

#####################
# Charts in Spanish #
#####################


###Death Charts
weekly_deaths_sp <- plot_ly(death_week, x = ~week_of, y = ~weekly_deaths, name = 'Muertes semenales', type = 'bar')
weekly_deaths_sp <- weekly_deaths_sp %>% add_trace(y = ~deaths_4wa, name = 'Promedio de 4 semanas', type = 'scatter', mode = 'lines', fill = 'tozeroy')
weekly_deaths_sp <- weekly_deaths_sp %>% layout(xaxis = list(title = "Fecha"),
                                          yaxis = list (title = "Muertes"),
                                          shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                             line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))


#Vaccination Status
vax_death_sp <- plot_ly(death_week, x = ~week_of, y = ~vaccinated, type = 'bar', name = 'Vacunados (2+ dosis')
vax_death_sp <- vax_death_sp %>% add_trace(y = ~unvaccinated, name = 'No vacunado')
vax_death_sp <- vax_death_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Muertes'), barmode = 'stack',
                                  shapes = list(list(type = "rect", text = "Deaths in this time may have not yet been reported", fillcolor = 'grey',
                                                     line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))

#County
county_death_sp <- plot_ly(death_week, x = ~week_of, y = ~`Box Elder`, type = 'bar', name = 'Box Elder')
county_death_sp <- county_death_sp %>% add_trace(y = ~Cache, name = 'Cache')
county_death_sp <- county_death_sp %>% add_trace(y = ~Rich, name = 'Rich')
county_death_sp <- county_death_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Muertes'), barmode = 'stack',
                                        shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                           line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))

#Age
age_death_sp <- plot_ly(death_week, x = ~week_of, y = ~`85+`, type = 'bar', name = '85+')
age_death_sp <- age_death_sp %>% add_trace(y = ~`65-84`, name = '65-84')
age_death_sp <- age_death_sp %>% add_trace(y = ~`45-64`, name = '45-64')
age_death_sp <- age_death_sp %>% add_trace(y = ~`25-44`, name = '25-44')
age_death_sp <- age_death_sp %>% add_trace(y = ~`15-24`, name = '15-24')
age_death_sp <- age_death_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Muertes'), barmode = 'stack',
                                  shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                     line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(death_week$weekly_deaths), x0 = todays_date - 14, x1 = todays_date)))

vax_death_month_sp <- plot_ly(death_month, x = ~Month, y = ~vaccinated, type = 'bar', name = 'Vacunados (2+ dosis')
vax_death_month_sp <- vax_death_month_sp %>% add_trace(y = ~unvaccinated, name = 'No vacunado')
vax_death_month_sp <- vax_death_month_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Deaths'), barmode = 'stack',
                                              shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                                 line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = 32, x0 = todays_date - 14, x1 = todays_date)))
###Hospitalization Charts

hosp_df <- hosp_week %>%
  dplyr::select(week_of, weekly_hosp, hosp_4wa)

weekly_hosp_sp <- plot_ly(hosp_df, x = ~week_of, y = ~weekly_hosp, name = 'Hospitalizaciones semanales', type = 'bar')
weekly_hosp_sp <- weekly_hosp_sp %>% add_trace(y = ~hosp_4wa, name = 'Promedios de 4 semanas', type = 'scatter', mode = 'lines', fill = 'tozeroy')
weekly_hosp_sp <- weekly_hosp_sp %>% layout(xaxis = list(title = "Fecha"),
                                      yaxis = list (title = "Hospitalizaciones"),
                                      shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                         line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))


#Vaccination Status
vax_hosp_sp <- plot_ly(hosp_week, x = ~week_of, y = ~vaccinated, type = 'bar', name = 'Vacunados (2+ dosis')
vax_hosp_sp <- vax_hosp_sp %>% add_trace(y = ~unvaccinated, name = 'No vacunado')
vax_hosp_sp <- vax_hosp_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Hospitalizaciones'), barmode = 'stack',
                                shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                   line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))

#County
county_hosp_sp <- plot_ly(hosp_week, x = ~week_of, y = ~`Box Elder`, type = 'bar', name = 'Box Elder')
county_hosp_sp <- county_hosp_sp %>% add_trace(y = ~Cache, name = 'Cache')
county_hosp_sp <- county_hosp_sp %>% add_trace(y = ~Rich, name = 'Rich')
county_hosp_sp <- county_hosp_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Hospitalizaciones'), barmode = 'stack',
                                      shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                         line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))

#Age
age_hosp_sp <- plot_ly(hosp_week, x = ~week_of, y = ~`85+`, type = 'bar', name = '85+')
age_hosp_sp <- age_hosp_sp %>% add_trace(y = ~`65-84`, name = '65-84')
age_hosp_sp <- age_hosp_sp %>% add_trace(y = ~`45-64`, name = '45-64')
age_hosp_sp <- age_hosp_sp %>% add_trace(y = ~`25-44`, name = '25-44')
age_hosp_sp <- age_hosp_sp %>% add_trace(y = ~`15-24`, name = '15-24')
age_hosp_sp <- age_hosp_sp %>% add_trace(y = ~`1-14`, name = '1-14')
age_hosp_sp <- age_hosp_sp %>% add_trace(y = ~`<1`, name = '<1')
age_hosp_sp <- age_hosp_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Hospitalizaciones'), barmode = 'stack',
                                shapes = list(list(type = "rect", text = "Es posible que aún no se hayan informado las muertes en este tiempo.", fillcolor = 'grey',
                                                   line = list(color = 'grey'), opacity = 0.2, y0= 0, y1 = max(hosp_week$weekly_hosp), x0 = todays_date - 7, x1 = last(hosp_week$week_of) + 7)))


###Case Charts
weekly_case_sp <- plot_ly(case_week, x = ~week_of, y = ~BRHD_new_week, type = 'bar', name = 'Casos Nuevos Semanales')
weekly_case_sp <- weekly_case_sp %>% add_trace(y = ~BRHD_4wa, name = 'Promedio de 4 semanas', type = 'scatter', mode = 'lines', fill = 'tozeroy')
weekly_case_sp <- weekly_case_sp %>% layout(xaxis = list(title = "Fecha"),
                                      yaxis = list (title = "Casos Nuevos Semanales"))
#weekly_case_sp <- plot_ly(case_week, x = ~week_of, y = ~BRHD_new_week, type = 'bar', name = 'Casos Nuevos Semanales')
#weeky_case_sp <- weekly_case_sp %>% add_trace(y = ~BRHD_4wa, name =  'Promedio de 4 semanas', type = 'scatter', mode = 'lines', fill = 'tozeroy')
#weekly_case_sp <- weekly_case_sp %>% layout(xaxis = list(title = "Fecha"),
                                    #yaxis = list (title = "Casos Nuevos Semanales"))
weekly_case_sp

county_percap_sp <- plot_ly(case_daily, x = ~Date, y = ~BRHD_pc7, type = 'scatter', mode = "lines", name = 'BRHD')
county_percap_sp <- county_percap_sp %>% add_trace(y = ~Box_Elder_pc7, name = 'Box Elder', mode = 'lines')
county_percap_sp <- county_percap_sp %>% add_trace(y = ~Cache_pc7, name = 'Cache', mode = 'lines')
county_percap_sp <- county_percap_sp %>% add_trace(y = ~Rich_pc7, name = 'Rich', mode = 'lines')
county_percap_sp <- county_percap_sp %>% layout(xaxis = list(title = "Fecha"),
                                          yaxis = list (title = "Casos Nuevos"))


county_rate_sp <- plot_ly(case_daily, x = ~Date, y = ~BRHD_7d, type = 'scatter', mode = "lines", name = 'BRHD')
county_rate_sp <- county_rate_sp %>% add_trace(y = ~Box_Elder_7d, name = 'Box Elder', mode = 'lines')
county_rate_sp <- county_rate_sp %>% add_trace(y = ~Cache_7d, name = 'Cache', mode = 'lines')
county_rate_sp <- county_rate_sp %>% add_trace(y = ~Rich_7d, name = 'Rich', mode = 'lines')
county_rate_sp <- county_rate_sp %>% layout(xaxis = list(title = "Fecha"),
                                      yaxis = list (title = "Casos Nuevos"))

county_srate_sp <- plot_ly(case_daily, x = ~Date, y = ~BRHD_pcrate, type = 'scatter', mode = "lines", name = 'BRHD')
county_srate_sp <- county_srate_sp %>% add_trace(y = ~Box_Elder_pcrate, name = 'Box Elder', mode = 'lines')
county_srate_sp <- county_srate_sp %>% add_trace(y = ~Cache_pcrate, name = 'Cache', mode = 'lines')
county_srate_sp <- county_srate_sp %>% add_trace(y = ~Rich_pcrate, name = 'Rich', mode = 'lines')
county_srate_sp <- county_srate_sp %>% layout(xaxis = list(title = "Fecha"),
                                        yaxis = list (title = "Tasa de casos"))

#7-day average by city crude numbers
city_sda_sp <- plot_ly(BRHD_city, x = ~Date, y = ~brigham_7da, name = 'Brigham City', type = 'scatter', mode = 'lines',
                    line = list(color = '#800000', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~garland_7da, name = 'Garland', line = list(color = '#000000', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~hyrum_7da, name = 'Hyrum', line = list(color = '#8B4513', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~lewiston_7da, name = 'Lewiston', line = list(color = '#E9967A', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~logan_7da, name = 'Logan', line = list(color = '#BC8F8F', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~mendon_7da, name = 'Mendon', line = list(color = '#FF1493', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~millville_7da, name = 'Millville', line = list(color = '#8B008B', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~northlogan_7da, name = 'North Logan', line = list(color = '#8A2BE2', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~paradise_7da, name = 'Paradise', line = list(color = '#00008B', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~perry_7da, name = 'Perry', line = list(color = '#008B8B', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~providence_7da, name = 'Providence', line = list(color = '#20B2AA', width = 2)) 
city_sda_sp <- city_sda_sp %>% add_trace(y = ~richmond_7da, name = 'Richmond', line = list(color = '#00FF7F', width = 2))
city_sda_sp <- city_sda_sp %>% add_trace(y = ~riverheights_7da, name = 'River Heights', line = list(color = '#008000', width = 2))
city_sda_sp <- city_sda_sp %>% add_trace(y = ~smithfield_7da, name = 'Smithfield', line = list(color = '#9ACD32', width = 2))
city_sda_sp <- city_sda_sp %>% add_trace(y = ~tremonton_7da, name = 'Tremonton', line = list(color = '#FFD700', width = 2))
city_sda_sp <- city_sda_sp %>% add_trace(y = ~wellsville_7da, name = 'Wellsville', line = list(color = '#FF8C00', width = 2))
city_sda_sp <- city_sda_sp %>% add_trace(y = ~willard_7da, name = 'Willard', line = list(color = '#FF6347', width = 2))
city_sda_sp <- city_sda_sp %>% layout(xaxis = list(title = "Fecha"),
                                yaxis = list (title = "Casos diarios"))

vax_case_sp <- plot_ly(case_week, x = ~week_of, y = ~vaccinated, type = 'bar', name = 'Vacunados (2+ dosis')
vax_case_sp <- vax_case_sp %>% add_trace(y = ~unvaccinated, name = 'No vacunado')
vax_case_sp <- vax_case_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Casos semenales'), barmode = 'stack')


reinf_case_sp <- plot_ly(case_week, x = ~week_of, y = ~reinfection, type = 'bar', name = 'Reinfeccion')
reinf_case_sp <- reinf_case_sp %>% add_trace(y = ~firstinfection, name = 'Primera infeccion')
reinf_case_sp <- reinf_case_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Casos semenales'), barmode = 'stack')

age_case_sp <- plot_ly(case_week, x = ~week_of, y = ~`85+`, type = 'bar', name = '85+')
age_case_sp <- age_case_sp %>% add_trace(y = ~`65-84`, name = '65-84')
age_case_sp <- age_case_sp %>% add_trace(y = ~`45-64`, name = '45-64')
age_case_sp <- age_case_sp %>% add_trace(y = ~`25-44`, name = '25-44')
age_case_sp <- age_case_sp %>% add_trace(y = ~`15-24`, name = '15-24')
age_case_sp <- age_case_sp %>% add_trace(y = ~`1-14`, name = '1-14')
age_case_sp <- age_case_sp %>% add_trace(y = ~`<1`, name = '<1')
age_case_sp <- age_case_sp %>% layout(xaxis = list(title = "Fecha"), yaxis = list(title = 'Casos semanales'), barmode = 'stack')

#Tests
person_tests_sp <- plot_ly(person_pp, x = ~Date, y = ~BRHD_7pp, type = 'scatter', mode = "lines", name = 'BRHD')
person_tests_sp <- person_tests_sp %>% add_trace(y = ~Box_Elder_7pp, name = 'Box Elder', mode = 'lines')
person_tests_sp <- person_tests_sp %>% add_trace(y = ~Cache_7pp, name = 'Cache', mode = 'lines')
person_tests_sp <- person_tests_sp %>% layout(xaxis = list(title = "Fecha"),
                                        yaxis = list (title = "Porcentaje de positividad"))

test_tests_sp <- plot_ly(test_pp, x = ~Date, y = ~BRHD_7pp, type = 'scatter', mode = "lines", name = 'BRHD 7-Day')
test_tests_sp <- test_tests_sp %>% add_trace(y = ~Box_Elder_7pp, name = 'Box Elder', mode = 'lines')
test_tests_sp <- test_tests_sp %>% add_trace(y = ~Cache_7pp, name = 'Cache', mode = 'lines')
test_tests_sp <- test_tests_sp %>% layout(xaxis = list(title = "Fecha"),
                                    yaxis = list (title = "Porcentaje de positividad"))

daily_tests_sp <- plot_ly(person_pp, x = ~Date, y = ~BRHD_new_tests, type = 'bar', name = 'New Tests')
daily_tests_sp <- daily_tests_sp %>% add_trace(y = ~BRHD_7da, name = '7-Day Average', type = 'scatter', mode = 'lines', fill = 'tozeroy')
daily_tests_sp <- daily_tests_sp %>% layout(xaxis = list(title = "Fecha"),
                                      yaxis = list (title = "Nuevas Pruebas"))

###Vaccine Charts
vax_chart_sp <- plot_ly(vax_daily, x = ~Date, y = ~BRHD_first_per_5up, type = 'scatter', mode = "lines", name = 'BRHD')
vax_chart_sp <- vax_chart_sp %>% add_trace(y = ~Cache_first_per_5up, name = 'Cache', mode = 'lines')
vax_chart_sp <- vax_chart_sp %>% add_trace(y = ~Box_Elder_first_per_5up, name = 'Box Elder', mode = 'lines')
vax_chart_sp <- vax_chart_sp %>% add_trace(y = ~Rich_first_per_5up, name = 'Rich', mode = 'lines')
vax_chart_sp <- vax_chart_sp %>% layout(xaxis = list(title = "Fecha"),
                                  yaxis = list (title = "Porcentaje"))

vax_chart1_sp <- plot_ly(vax_daily, x = ~Date, y = ~BRHD_second_per_5up, type = 'scatter', mode = "lines", name = 'BRHD')
vax_chart1_sp <- vax_chart1_sp %>% add_trace(y = ~Cache_second_per_5up, name = 'Cache', mode = 'lines')
vax_chart1_sp <- vax_chart1_sp %>% add_trace(y = ~Box_Elder_second_per_5up, name = 'Box Elder', mode = 'lines')
vax_chart1_sp <- vax_chart1_sp %>% add_trace(y = ~Rich_second_per_5up, name = 'Rich', mode = 'lines')
vax_chart1_sp <- vax_chart1_sp %>% layout(xaxis = list(title = "Fecha"),
                                    yaxis = list (title = "Porcentaje"))

vax_chart2_sp <- plot_ly(vax_daily, x = ~Date, y = ~BRHD_third_per_12up, type = 'scatter', mode = "lines", name = 'BRHD')
vax_chart2_sp <- vax_chart2_sp %>% add_trace(y = ~Cache_third_per_12up, name = 'Cache', mode = 'lines')
vax_chart2_sp <- vax_chart2_sp %>% add_trace(y = ~Box_Elder_third_per_12up, name = 'Box Elder', mode = 'lines')
vax_chart2_sp <- vax_chart2_sp %>% add_trace(y = ~Rich_third_per_12up, name = 'Rich', mode = 'lines')
vax_chart2_sp <- vax_chart2_sp %>% layout(xaxis = list(title = "Fecha"),
                                    yaxis = list (title = "Porcentaje"))


#Save English Items
tmp.env <- new.env()

tmp.env$weekly_deaths <- weekly_deaths
tmp.env$weekly_hosp <- weekly_hosp
tmp.env$weekly_case <- weekly_case
tmp.env$vax_death_month <- vax_death_month
tmp.env$county_death <- county_death
tmp.env$age_death <- age_death
tmp.env$vax_hosp <- vax_hosp
tmp.env$county_hosp <- county_hosp
tmp.env$age_hosp <- age_hosp
tmp.env$county_rate <- county_rate
tmp.env$county_percap <- county_percap
tmp.env$county_srate <- county_srate
tmp.env$city_sda <- city_sda
tmp.env$vax_case <- vax_case
tmp.env$reinf_case <- reinf_case
tmp.env$age_case <- age_case
tmp.env$test_tests <- test_tests
tmp.env$person_tests <- person_tests
tmp.env$vax_chart <- vax_chart
tmp.env$vax_chart1 <- vax_chart1
tmp.env$vax_chart2 <- vax_chart2
tmp.env$total <- total
tmp.env$total_vax <- total_vax
tmp.env$daily_tests <- daily_tests

save(list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env,
     file = paste0("Ecasedash",
                   Sys.Date(), ".Rdata"))

save(list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env,
     file = "C:/Users/tstoker/Desktop/RWD/new_dash/Ecasedash.Rdata")

rm(tmp.env)

#Save Spanish Items
sp.env <- new.env()

sp.env$weekly_deaths_sp <- weekly_deaths_sp
sp.env$weekly_hosp_sp <- weekly_hosp_sp
sp.env$weekly_case_sp <- weekly_case_sp
sp.env$vax_death_month_sp <- vax_death_month_sp
sp.env$county_death_sp <- county_death_sp
sp.env$age_death_sp <- age_death_sp
sp.env$vax_hosp_sp <- vax_hosp_sp
sp.env$county_hosp_sp <- county_hosp_sp
sp.env$age_hosp_sp <- age_hosp_sp
sp.env$county_rate_sp <- county_rate_sp
sp.env$county_percap_sp <- county_percap_sp
sp.env$county_srate_sp <- county_srate_sp
sp.env$city_sda_sp <- city_sda_sp
sp.env$vax_case_sp <- vax_case_sp
sp.env$reinf_case_sp <- reinf_case_sp
sp.env$age_case_sp <- age_case_sp
sp.env$test_tests_sp <- test_tests_sp
sp.env$person_tests_sp <- person_tests_sp
sp.env$daily_tests_sp <- daily_tests_sp
sp.env$vax_chart_sp <- vax_chart_sp
sp.env$vax_chart1_sp <- vax_chart1_sp
sp.env$vax_chart2_sp <- vax_chart2_sp
sp.env$total_sp <- total_sp
sp.env$total_vaxsp <- total_vaxsp

save(list = ls(all.names = TRUE, pos = sp.env), envir = sp.env,
     file = paste0("Ecasedash",
                   Sys.Date(), ".Rdata"))

save(list = ls(all.names = TRUE, pos = sp.env), envir = sp.env,
     file = "C:/Users/tstoker/Desktop/RWD/SP_new/Ecasedash.Rdata")

rm(sp.env)

########################################
#Hospital Cases
########################################
lasthosp <- read.csv("lasthosp.csv", header = T)

lasthosp$first_positive_lab <- as.Date(lasthosp$first_positive_lab)
lasthosp$hosp_admit_date <- as.Date(lasthosp$hosp_admit_date)

#Merge old and new k12 complete cases
hosp_merged <- merge(lasthosp, hosp, all = TRUE)


#Remove duplicates, leaving only new complete k12 cases that are ready to send to school
hosp_new <- hosp_merged[!(duplicated(hosp_merged$patient_record_number) | duplicated(hosp_merged$patient_record_number, fromLast = TRUE)), ] %>% 
  dplyr::select(patient_record_number, hosp_admit_date, first_positive_lab, condition_caused_death, covid_death)


##ONLY WRITE IF THE ABOVE STEP WORKED###
#Write current k12 data set to working directory for future comparisons
write.csv(hosp, "lasthosp.csv")
write.csv(hosp_new, "hosp_new.csv")







