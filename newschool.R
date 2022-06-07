#udohcases <-  read.csv("udoh105.csv", header = T) %>% 
  #dplyr::select(patient_record_number, first_positive_lab)

#epitraxudoh_merge <-merge(x = udohcases, y = export_epicases,   
#                          by.x = "patient_record_number", by.y = "patient_record_number", all = TRUE)

#lastk12_merge <-merge(x = lastk12, y = k12comp,   
                         # by.x = "patient_record_number", by.y = "patient_record_number", all = TRUE)
#x <- merge(lastk12, k12comp, all = TRUE) %>% 
  
#x <- x[!(duplicated(x$patient_record_number) | duplicated(x$patient_record_number, fromLast = TRUE)), ]

#lastk12 <-lastk12 %>%
  #dplyr::select(patient_record_number)

#common_cols <- intersect(colnames(lastk12), colnames(lastk12_merge))

#invsmerged=rbind(subset(lastk12_merge, select = common_cols),
                 #subset(lastk12, select = common_cols))

#Remove duplicates
#lk12 <- invsmerged[!(duplicated(invsmerged$patient_record_number) | duplicated(invsmerged$patient_record_number, fromLast = TRUE)), ]
#write.csv(lk12, "C:/Users/tstoker/Desktop/RWD/lk12.csv")
#write.csv(lastk12_merge, "C:/Users/tstoker/Desktop/RWD/lastk12_merge.csv")
rm(list=ls())  
#########
setwd("C:/Users/tstoker/Desktop/RWD")
#Load tidyverse, specifically using DPLYR, STRINGR, GGPLOT
#library(tidyverse)

#Unknown
library(dplyr)
library(stringr)
library(ggplot2)
library(raster)
library(tidyquant)
library(tidyr)
library(reactable)
library(plotly)
library(stringi)

#Enrollment numbers from:
#https://www.schools.utah.gov/data/reports?mid=1424&tid=6 


##################################################
# Clean Case Data- Only Run for Afternoon Update #
##################################################
todays_date <- Sys.Date()
export_epicases <- read.csv("epicases67.csv", header = T)

#Format dates for case export
export_epicases$covid_case_date = as.Date.character(export_epicases$covid_case_date)
export_epicases$patient_date_of_death = as.Date.character(strsplit(as.character(strptime(export_epicases$patient_date_of_death,"%Y-%m-%d"))," "))
export_epicases$patient_event_onset_date <- as.Date(export_epicases$patient_event_onset_date)

#Filters to confirmed cases and probable\confirmed deaths in Bear River District
export_epicases <- export_epicases %>% 
  filter(patient_jurisdiction_of_residence %in% c("Bear River","")) %>% 
  filter(patient_state_case_status == "Confirmed" | condition_caused_death == "Yes") %>% 
  #Removes duplicate CMRs, prioritizing CMRs with "Inpatient" visit type over "", "Unknown", or "Outpatient"
  arrange(factor(person_facility_visit_type, levels = c("Inpatient", "Outpatient", "Unknown", NA))) %>% 
  distinct(patient_record_number, .keep_all = TRUE) %>% 
  #Creates week variable using epiweek, where a week is Sun-Sat
  mutate(week = epiweek(covid_case_date), yr = year(covid_case_date),
         week = if_else(week < 53 & yr == 2021, week + 53, week),
         `Week Of` = as.Date("2019-12-22") + (7*week)) %>% 
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

#Cleans hospitalization data
export_epicases <- export_epicases %>% 
  mutate(Hospitalized = case_when(corona_pui_col_hosp_ncov == "Yes" ~ 1,
                                  corona_pui_col_icu == "Yes" ~ 1,
                                  person_facility_visit_type == "Inpatient" ~ 1,
                                  TRUE ~ 0))
#######################################################################################
#Merge Datasets step to get test collection variable
#########################################################################################
export_cases <- read.csv("cases67.csv", header = T) %>% 
  dplyr::select(patient_record_number, first_positive_lab, case_onset_date) %>% 
  replace(is.na(.),"")


epitraxudoh_merge <-merge(x = export_cases, y = export_epicases,   
                          by.x = "patient_record_number", by.y = "patient_record_number", all = TRUE)

############################################################################################
#Create Dataset for every school-associated case since the start of the '21-'22 schoolyear


###################################################################################
todays_date <- Sys.Date()

k12 <- epitraxudoh_merge %>% 
  filter(covid_case_date >= "2021-08-18") %>%
  filter(corona_pui_col_school_univ_type != "Volunteer/visitor") %>% 
  filter(corona_pui_col_school_univ_type != "volunteer-visitor") %>% 
  filter(exposure_place_school_district %in% c("Bear River Charter School","Box Elder District","Bridgerland Technical College",
                                               "Cache District","The Center for Creativity Innovation and Discovery","Edith Bowen Laboratory School","Fast Forward High","Intech Collegiate Academy",
                                               "Logan City District","Promontory School of Expeditionary Learning","Rich District","Thomas Edison","Thomas Edison - South - Thomas Edison", "Thomas Edison Charter Schools South", "Thomas Edison Charter School South")) %>% 
  #Rename District to what we'll use for the dashboard
  mutate(District = case_when(exposure_place_school_district == "Box Elder District" ~ "Box Elder School District",
                              exposure_place_school_district == "Cache District" ~ "Cache County School District",
                              exposure_place_school_district == "Logan City District" ~ "Logan City School District",
                              exposure_place_school_district == "Rich District" ~ "Rich School District",
                              TRUE ~ "Charter/Other School"))
  
#Format onset date to remove hours
k12$first_positive_lab <- as.Date(k12$first_positive_lab)

#reformat birth date
k12$patient_birth_date <- as.Date.character(strsplit(as.character(strptime(k12$patient_birth_date,"%Y-%m-%d"))," "))



#Filter to K-12 Student cases (the only ones that count against the threshold)
k12stud <- k12 %>% 
  filter(patient_age_at_event_in_years <= 18, patient_age_at_event_in_years >= 5)

#Filter to K-12 cases that were likely caused by school transmission
k12trans <- k12 %>% 
  filter(corona_pui_col_school_acquired_case == "Yes")

#Create Data Shell of every school (67 of them), for every day since the start of the 2021-2022 school year
k12_daily <- bind_cols(Date = rep(seq.Date(as.Date("2021-08-18"), todays_date, by=1),67),
                       #School names, starting with Box Elder
                       School = c(rep("Adele C. Young Intermediate", todays_date - as.Date("2021-08-17")),
                                  rep("Bear River High School", todays_date - as.Date("2021-08-17")),
                                  rep("Bear River Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("Box Elder High School", todays_date - as.Date("2021-08-17")),
                                  rep("Box Elder Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("Alice C. Harris Intermediate", todays_date - as.Date("2021-08-17")),
                                  rep("Century Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Sunrise High School", todays_date - as.Date("2021-08-17")),
                                  rep("Discovery Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Fielding Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Foothill Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Garland Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Grouse Creek Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Lake View Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("McKinley Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Mountain View Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("North Park Elementary (Tremonton)", todays_date - as.Date("2021-08-17")),
                                  rep("Park Valley Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Snowville Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Three Mile Creek Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Willard Elementary", todays_date - as.Date("2021-08-17")),
                                  #Cache Schools
                                  rep("Cache Alternative High School", todays_date - as.Date("2021-08-17")),
                                  rep("Green Canyon High School", todays_date - as.Date("2021-08-17")),
                                  rep("Mountain Crest High School", todays_date - as.Date("2021-08-17")),
                                  rep("Ridgeline High School", todays_date - as.Date("2021-08-17")),
                                  rep("Sky View High School", todays_date - as.Date("2021-08-17")),
                                  rep("Birch Creek Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Canyon Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Cedar Ridge Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Greenville Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Heritage Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Lewiston Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Lincoln Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Millville Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Mountainside Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Nibley Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("North Cache Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("North Park Elementary (North Logan)", todays_date - as.Date("2021-08-17")),
                                  rep("Providence Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("River Heights Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("South Cache Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("Spring Creek Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("Summit Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Sunrise Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Wellsville Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("White Pine Elementary", todays_date - as.Date("2021-08-17")),
                                  #Logan Schools
                                  rep("Logan High School", todays_date - as.Date("2021-08-17")),
                                  rep("Mount Logan Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("Adams Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Bridger Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Ellis Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Hillcrest Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Wilson Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("Woodruff Elementary", todays_date - as.Date("2021-08-17")),
                                  #Rich Schools
                                  rep("Rich High School", todays_date - as.Date("2021-08-17")),
                                  rep("Rich Middle School", todays_date - as.Date("2021-08-17")),
                                  rep("North Rich Elementary", todays_date - as.Date("2021-08-17")),
                                  rep("South Rich Elementary", todays_date - as.Date("2021-08-17")),
                                  #Charter/Other Schools
                                  rep("Bear River Charter School", todays_date - as.Date("2021-08-17")),
                                  rep("Bridgerland Technical College", todays_date - as.Date("2021-08-17")),
                                  rep("The Center for Creativity Innovation and Discovery", todays_date - as.Date("2021-08-17")),
                                  rep("Edith Bowen Laboratory School", todays_date - as.Date("2021-08-17")),
                                  rep("Thomas Edison North", todays_date - as.Date("2021-08-17")),
                                  rep("Thomas Edison South", todays_date - as.Date("2021-08-17")),
                                  rep("Fast Forward High", todays_date - as.Date("2021-08-17")),
                                  rep("Intech Collegiate Academy", todays_date - as.Date("2021-08-17")),
                                  rep("Promontory School of Expeditionary Learning", todays_date - as.Date("2021-08-17"))),
                       #Enrollment numbers in the same order, starting with Box Elder
                       Enrollment = c(rep(1129, todays_date - as.Date("2021-08-17")),
                                      rep(1153, todays_date - as.Date("2021-08-17")),
                                      rep(900, todays_date - as.Date("2021-08-17")),
                                      rep(1550, todays_date - as.Date("2021-08-17")),
                                      rep(1116, todays_date - as.Date("2021-08-17")),
                                      rep(821, todays_date - as.Date("2021-08-17")),
                                      rep(502, todays_date - as.Date("2021-08-17")),
                                      rep(192, todays_date - as.Date("2021-08-17")),
                                      rep(579, todays_date - as.Date("2021-08-17")),
                                      rep(581, todays_date - as.Date("2021-08-17")),
                                      rep(452, todays_date - as.Date("2021-08-17")),
                                      rep(687, todays_date - as.Date("2021-08-17")),
                                      rep(8, todays_date - as.Date("2021-08-17")),
                                      rep(600, todays_date - as.Date("2021-08-17")),
                                      rep(603, todays_date - as.Date("2021-08-17")),
                                      rep(292, todays_date - as.Date("2021-08-17")),
                                      rep(552, todays_date - as.Date("2021-08-17")),
                                      rep(35, todays_date - as.Date("2021-08-17")),
                                      rep(32, todays_date - as.Date("2021-08-17")),
                                      rep(575, todays_date - as.Date("2021-08-17")),
                                      rep(342, todays_date - as.Date("2021-08-17")),
                                      #Cache Schools
                                      rep(145, todays_date - as.Date("2021-08-17")),
                                      rep(1584, todays_date - as.Date("2021-08-17")),
                                      rep(1484, todays_date - as.Date("2021-08-17")),
                                      rep(1708, todays_date - as.Date("2021-08-17")),
                                      rep(1439, todays_date - as.Date("2021-08-17")),
                                      rep(600, todays_date - as.Date("2021-08-17")),
                                      rep(664, todays_date - as.Date("2021-08-17")),
                                      rep(702, todays_date - as.Date("2021-08-17")),
                                      rep(509, todays_date - as.Date("2021-08-17")),
                                      rep(734, todays_date - as.Date("2021-08-17")),
                                      rep(441, todays_date - as.Date("2021-08-17")),
                                      rep(458, todays_date - as.Date("2021-08-17")),
                                      rep(454, todays_date - as.Date("2021-08-17")),
                                      rep(418, todays_date - as.Date("2021-08-17")),
                                      rep(408, todays_date - as.Date("2021-08-17")),
                                      rep(1137, todays_date - as.Date("2021-08-17")),
                                      rep(509, todays_date - as.Date("2021-08-17")),
                                      rep(751, todays_date - as.Date("2021-08-17")),
                                      rep(525, todays_date - as.Date("2021-08-17")),
                                      rep(1022, todays_date - as.Date("2021-08-17")),
                                      rep(787, todays_date - as.Date("2021-08-17")),
                                      rep(673, todays_date - as.Date("2021-08-17")),
                                      rep(759, todays_date - as.Date("2021-08-17")),
                                      rep(502, todays_date - as.Date("2021-08-17")),
                                      rep(396, todays_date - as.Date("2021-08-17")),
                                      #Logan Schools
                                      rep(1548, todays_date - as.Date("2021-08-17")),
                                      rep(1167, todays_date - as.Date("2021-08-17")),
                                      rep(252, todays_date - as.Date("2021-08-17")),
                                      rep(467, todays_date - as.Date("2021-08-17")),
                                      rep(245, todays_date - as.Date("2021-08-17")),
                                      rep(419, todays_date - as.Date("2021-08-17")),
                                      rep(495, todays_date - as.Date("2021-08-17")),
                                      rep(606, todays_date - as.Date("2021-08-17")),
                                      #Rich Schools
                                      rep(151, todays_date - as.Date("2021-08-17")),
                                      rep(125, todays_date - as.Date("2021-08-17")),
                                      rep(118, todays_date - as.Date("2021-08-17")),
                                      rep(117, todays_date - as.Date("2021-08-17")),
                                      #Charter/Other Schools
                                      rep(161, todays_date - as.Date("2021-08-17")),
                                      rep(NA, todays_date - as.Date("2021-08-17")),
                                      rep(485, todays_date - as.Date("2021-08-17")),
                                      rep(357, todays_date - as.Date("2021-08-17")),
                                      rep(NA, todays_date - as.Date("2021-08-17")),
                                      rep(650, todays_date - as.Date("2021-08-17")),
                                      rep(355, todays_date - as.Date("2021-08-17")),
                                      rep(199, todays_date - as.Date("2021-08-17")),
                                      rep(448, todays_date - as.Date("2021-08-17"))))

#Create Districts
k12_daily <- k12_daily %>% 
  #Starting with Box Elder School District
  mutate(District = case_when(School %in% c("Adele C. Young Intermediate","Bear River High School","Bear River Middle School","Box Elder High School", 
                                            "Box Elder Middle School","Alice C. Harris Intermediate","Century Elementary","Sunrise High School", 
                                            "Discovery Elementary","Fielding Elementary","Foothill Elementary","Garland Elementary","Grouse Creek Elementary", 
                                            "Lake View Elementary","McKinley Elementary","Mountain View Elementary","North Park Elementary (Tremonton)","Park Valley Elementary", 
                                            "Snowville Elementary","Three Mile Creek Elementary","Willard Elementary") ~ "Box Elder School District",
                              #Cache County School District
                              School %in% c("Cache Alternative High School","Green Canyon High School","Mountain Crest High School","Ridgeline High School", 
                                            "Sky View High School","Birch Creek Elementary","Canyon Elementary","Cedar Ridge Elementary","Greenville Elementary", 
                                            "Heritage Elementary","Lewiston Elementary","Lincoln Elementary","Millville Elementary","Mountainside Elementary", 
                                            "Nibley Elementary","North Cache Middle School","North Park Elementary (North Logan)","Providence Elementary","River Heights Elementary", 
                                            "South Cache Middle School","Spring Creek Middle School","Summit Elementary","Sunrise Elementary","Wellsville Elementary", 
                                            "White Pine Elementary") ~ "Cache County School District",
                              #Logan City School District
                              School %in% c("Logan High School","Mount Logan Middle School","Adams Elementary","Bridger Elementary","Ellis Elementary",
                                            "Hillcrest Elementary","Wilson Elementary","Woodruff Elementary") ~ "Logan City School District",
                              #Rich School District
                              School %in% c("Rich High School","Rich Middle School","North Rich Elementary","South Rich Elementary") ~ "Rich School District",
                              #Charter/Other Schools
                              School %in% c("Bear River Charter School","Bridgerland Technical College","The Center for Creativity Innovation and Discovery","Edith Bowen Laboratory School","Thomas Edison North", 
                                            "Thomas Edison South","Fast Forward High","Intech Collegiate Academy","Promontory School of Expeditionary Learning") ~ "Charter/Other Schools"))



#Summarize student cases into the k-12 daily dataframe
k12_grouped <- left_join(k12_daily, k12stud %>%
                           mutate(School = case_when(place_name == "Adele C. Young Intermediate - Box Elder District" ~ "Adele C. Young Intermediate", 
                                                     place_name == "Bear River High - Box Elder District" ~ "Bear River High School",
                                                     place_name == "Bear River Middle - Box Elder District" ~ "Bear River Middle School",
                                                     place_name == "Box Elder High - Box Elder District" ~ "Box Elder High School",
                                                     place_name == "Box Elder Middle - Box Elder District" ~ "Box Elder Middle School",
                                                     place_name == "Alice C. Harris Intermediate - Box Elder District" ~ "Alice C. Harris Intermediate",
                                                     place_name == "Century Elementary School - Box Elder District" ~ "Century Elementary",
                                                     place_name == "Sunrise High Elementary School - Box Elder District" ~ "Sunrise High School",
                                                     place_name == "Discovery Elementary School - Box Elder District" ~ "Discovery Elementary",
                                                     place_name == "Fielding Elementary School - Box Elder District" ~ "Fielding Elementary",
                                                     place_name == "Foothill Elementary School - Box Elder District" ~ "Foothill Elementary",
                                                     place_name == "Garland Elementary School - Box Elder District" ~ "Garland Elementary",
                                                     place_name == "Grouse Creek Elementary School - Box Elder District" ~ "Grouse Creek Elementary",
                                                     place_name == "Lake View Elementary School - Box Elder District" ~ "Lake View Elementary",
                                                     place_name == "McKinley Elementary School - Box Elder District" ~ "McKinley Elementary",
                                                     place_name == "Mountain View Elementary School - Box Elder District" ~ "Mountain View Elementary",
                                                     place_name == "North Park Elementary School - Box Elder District" ~ "North Park Elementary (Tremonton)",
                                                     place_name == "Park Valley Elementary School - Box Elder District" ~ "Park Valley Elementary",
                                                     place_name == "Snowville Elementary School - Box Elder District" ~ "Snowville Elementary",
                                                     place_name == "Three Mile Creek Elementary School - Box Elder District" ~ "Three Mile Creek Elementary",
                                                     place_name == "Willard Elementary School - Box Elder District" ~ "Willard Elementary",
                                                     #Cache Schools
                                                     place_name == "Cache Alternative High - Cache District" ~ "Cache Alternative High School",
                                                     place_name == "Green Canyon High School - Cache District" ~ "Green Canyon High School",
                                                     place_name == "Mountain Crest High - Cache District" ~ "Mountain Crest High School",
                                                     place_name == "Ridgeline High School - Cache District" ~ "Ridgeline High School",
                                                     place_name == "Sky View High - Cache District" ~ "Sky View High School",
                                                     place_name == "Birch Creek Elementary School - Cache District" ~ "Birch Creek Elementary",
                                                     place_name == "Canyon Elementary School - Cache District" ~ "Canyon Elementary",
                                                     place_name == "Cedar Ridge Elementary School - Cache District" ~ "Cedar Ridge Elementary",
                                                     place_name == "Greenville Elementary School - Cache District" ~ "Greenville Elementary",
                                                     place_name == "Heritage Elementary School - Cache District" ~ "Heritage Elementary",
                                                     place_name == "Lewiston Elementary School - Cache District" ~ "Lewiston Elementary",
                                                     place_name == "Lincoln Elementary School - Cache District" ~ "Lincoln Elementary",
                                                     place_name == "Millville Elementary School - Cache District" ~ "Millville Elementary",
                                                     place_name == "Mountainside Elementary School - Cache District" ~ "Mountainside Elementary",
                                                     place_name == "Nibley Elementary School - Cache District" ~ "Nibley Elementary",
                                                     place_name == "North Cache Middle School - Cache District" ~ "North Cache Middle School",
                                                     place_name == "North Park Elementary School - Cache District" ~ "North Park Elementary (North Logan)",
                                                     place_name == "Providence Elementary School - Cache District" ~ "Providence Elementary",
                                                     place_name == "River Heights Elementary School - Cache District" ~ "River Heights Elementary",
                                                     place_name == "South Cache Middle School - Cache District" ~ "South Cache Middle School",
                                                     place_name == "Spring Creek Middle School - Cache District" ~ "Spring Creek Middle School",
                                                     place_name == "Summit Elementary School - Cache District" ~ "Summit Elementary",
                                                     place_name == "Sunrise Elementary School - Cache District" ~ "Sunrise Elementary",
                                                     place_name == "Wellsville Elementary School - Cache District" ~ "Wellsville Elementary",
                                                     place_name == "White Pine Elementary School - Cache District" ~ "White Pine Elementary",
                                                     #Logan Schools
                                                     place_name == "Logan High - Logan City District" ~ "Logan High School",
                                                     place_name == "Mount Logan Middle - Logan City District" ~ "Mount Logan Middle School",
                                                     place_name == "Adams Elementary School - Logan City District" ~ "Adams Elementary",
                                                     place_name == "Bridger Elementary School - Logan City District" ~ "Bridger Elementary",
                                                     place_name == "Ellis Elementary School - Logan City District" ~ "Ellis Elementary",
                                                     place_name == "Hillcrest Elementary School - Logan City District" ~ "Hillcrest Elementary",
                                                     place_name == "Wilson Elementary School - Logan City District" ~ "Wilson Elementary",
                                                     place_name == "Woodruff Elementary School - Logan City District" ~ "Woodruff Elementary",
                                                     #Rich Schools
                                                     place_name == "Rich High - Rich District" ~ "Rich High School",
                                                     place_name == "Rich Middle School - Rich District" ~ "Rich Middle School",
                                                     place_name == "North Rich Elementary School - Rich District" ~ "North Rich Elementary",
                                                     place_name == "South Rich Elementary School - Rich District" ~ "South Rich Elementary",
                                                     #Charter/Other Schools, only change Thomas Edison North since other schools can keep their place_name value
                                                     place_name == "Thomas Edison" ~ "Thomas Edison North",
                                                     place_name == "Thomas Edison Charter Schools South" ~ "Thomas Edison South",
                                                     place_name == "Thomas Edison - South - Thomas Edison" ~ "Thomas Edison South",
                                                     #If none of the above, school name is the place_name value (should only apply for the other charter schools)
                                                     TRUE ~ "as.character"(place_name))) %>% 
                           group_by(covid_case_date, School) %>% 
                           rename(Date = covid_case_date) %>% 
                           summarize(`New Student Cases` = n()), by = c("Date", "School")) %>% 
  mutate(`New Student Cases` = replace_na(`New Student Cases`, 0)) %>% group_by(School) %>% 
  mutate(`Total Student Cases` = cumsum(`New Student Cases`))



# Creates Current Threshold Cases (defined as all student cases in the past 7 days) and Threshold percent
k12_grouped <- left_join(k12_grouped, k12stud %>% 
                           mutate(threshold_date = covid_case_date + 7) %>% 
                           rename(Date = threshold_date) %>% 
                           mutate(School = case_when(place_name == "Adele C. Young Intermediate - Box Elder District" ~ "Adele C. Young Intermediate", 
                                                     place_name == "Bear River High - Box Elder District" ~ "Bear River High School",
                                                     place_name == "Bear River Middle - Box Elder District" ~ "Bear River Middle School",
                                                     place_name == "Box Elder High - Box Elder District" ~ "Box Elder High School",
                                                     place_name == "Box Elder Middle - Box Elder District" ~ "Box Elder Middle School",
                                                     place_name == "Alice C. Harris Intermediate - Box Elder District" ~ "Alice C. Harris Intermediate",
                                                     place_name == "Century Elementary School - Box Elder District" ~ "Century Elementary",
                                                     place_name == "Sunrise High Elementary School - Box Elder District" ~ "Sunrise High School",
                                                     place_name == "Discovery Elementary School - Box Elder District" ~ "Discovery Elementary",
                                                     place_name == "Fielding Elementary School - Box Elder District" ~ "Fielding Elementary",
                                                     place_name == "Foothill Elementary School - Box Elder District" ~ "Foothill Elementary",
                                                     place_name == "Garland Elementary School - Box Elder District" ~ "Garland Elementary",
                                                     place_name == "Grouse Creek Elementary School - Box Elder District" ~ "Grouse Creek Elementary",
                                                     place_name == "Lake View Elementary School - Box Elder District" ~ "Lake View Elementary",
                                                     place_name == "McKinley Elementary School - Box Elder District" ~ "McKinley Elementary",
                                                     place_name == "Mountain View Elementary School - Box Elder District" ~ "Mountain View Elementary",
                                                     place_name == "North Park Elementary School - Box Elder District" ~ "North Park Elementary (Tremonton)",
                                                     place_name == "Park Valley Elementary School - Box Elder District" ~ "Park Valley Elementary",
                                                     place_name == "Snowville Elementary School - Box Elder District" ~ "Snowville Elementary",
                                                     place_name == "Three Mile Creek Elementary School - Box Elder District" ~ "Three Mile Creek Elementary",
                                                     place_name == "Willard Elementary School - Box Elder District" ~ "Willard Elementary",
                                                     #Cache Schools
                                                     place_name == "Cache Alternative High - Cache District" ~ "Cache Alternative High School",
                                                     place_name == "Green Canyon High School - Cache District" ~ "Green Canyon High School",
                                                     place_name == "Mountain Crest High - Cache District" ~ "Mountain Crest High School",
                                                     place_name == "Ridgeline High School - Cache District" ~ "Ridgeline High School",
                                                     place_name == "Sky View High - Cache District" ~ "Sky View High School",
                                                     place_name == "Birch Creek Elementary School - Cache District" ~ "Birch Creek Elementary",
                                                     place_name == "Canyon Elementary School - Cache District" ~ "Canyon Elementary",
                                                     place_name == "Cedar Ridge Elementary School - Cache District" ~ "Cedar Ridge Elementary",
                                                     place_name == "Greenville Elementary School - Cache District" ~ "Greenville Elementary",
                                                     place_name == "Heritage Elementary School - Cache District" ~ "Heritage Elementary",
                                                     place_name == "Lewiston Elementary School - Cache District" ~ "Lewiston Elementary",
                                                     place_name == "Lincoln Elementary School - Cache District" ~ "Lincoln Elementary",
                                                     place_name == "Millville Elementary School - Cache District" ~ "Millville Elementary",
                                                     place_name == "Mountainside Elementary School - Cache District" ~ "Mountainside Elementary",
                                                     place_name == "Nibley Elementary School - Cache District" ~ "Nibley Elementary",
                                                     place_name == "North Cache Middle School - Cache District" ~ "North Cache Middle School",
                                                     place_name == "North Park Elementary School - Cache District" ~ "North Park Elementary (North Logan)",
                                                     place_name == "Providence Elementary School - Cache District" ~ "Providence Elementary",
                                                     place_name == "River Heights Elementary School - Cache District" ~ "River Heights Elementary",
                                                     place_name == "South Cache Middle School - Cache District" ~ "South Cache Middle School",
                                                     place_name == "Spring Creek Middle School - Cache District" ~ "Spring Creek Middle School",
                                                     place_name == "Summit Elementary School - Cache District" ~ "Summit Elementary",
                                                     place_name == "Sunrise Elementary School - Cache District" ~ "Sunrise Elementary",
                                                     place_name == "Wellsville Elementary School - Cache District" ~ "Wellsville Elementary",
                                                     place_name == "White Pine Elementary School - Cache District" ~ "White Pine Elementary",
                                                     #Logan Schools
                                                     place_name == "Logan High - Logan City District" ~ "Logan High School",
                                                     place_name == "Mount Logan Middle - Logan City District" ~ "Mount Logan Middle School",
                                                     place_name == "Adams Elementary School - Logan City District" ~ "Adams Elementary",
                                                     place_name == "Bridger Elementary School - Logan City District" ~ "Bridger Elementary",
                                                     place_name == "Ellis Elementary School - Logan City District" ~ "Ellis Elementary",
                                                     place_name == "Hillcrest Elementary School - Logan City District" ~ "Hillcrest Elementary",
                                                     place_name == "Wilson Elementary School - Logan City District" ~ "Wilson Elementary",
                                                     place_name == "Woodruff Elementary School - Logan City District" ~ "Woodruff Elementary",
                                                     #Rich Schools
                                                     place_name == "Rich High - Rich District" ~ "Rich High School",
                                                     place_name == "Rich Middle School - Rich District" ~ "Rich Middle School",
                                                     place_name == "North Rich Elementary School - Rich District" ~ "North Rich Elementary",
                                                     place_name == "South Rich Elementary School - Rich District" ~ "South Rich Elementary",
                                                     #Charter/Other Schools, only change Thomas Edison North since other schools can keep their place_name value
                                                     place_name == "Thomas Edison" ~ "Thomas Edison North",
                                                     place_name == "Thomas Edison Charter Schools South" ~ "Thomas Edison South",
                                                     place_name == "Thomas Edison - South - Thomas Edison" ~ "Thomas Edison South",
                                                     #If none of the above, school name is the place_name value (should only apply for the other charter schools)
                                                     TRUE ~ "as.character"(place_name))) %>%
                           group_by(Date, School) %>% 
                           summarize(n = n()), by = c("Date", "School")) %>% 
  mutate(n = replace_na(n, 0)) %>% group_by(School) %>% mutate(`Students after 7` = cumsum(n), n = NULL, 
                                                               `Current Threshold Cases` = `Total Student Cases` - `Students after 7`,
                                                               `Percent of Enrollment` = round((`Current Threshold Cases`/Enrollment)*100,2))

k12$patient_event_onset_date <- as.Date(k12$patient_event_onset_date)

#Add Variables for Total Cases (Including Staff, Teachers, etc.)
k12_grouped <- left_join(k12_grouped, k12 %>%
                           mutate(School = case_when(place_name == "Adele C. Young Intermediate - Box Elder District" ~ "Adele C. Young Intermediate", 
                                                     place_name == "Bear River High - Box Elder District" ~ "Bear River High School",
                                                     place_name == "Bear River Middle - Box Elder District" ~ "Bear River Middle School",
                                                     place_name == "Box Elder High - Box Elder District" ~ "Box Elder High School",
                                                     place_name == "Box Elder Middle - Box Elder District" ~ "Box Elder Middle School",
                                                     place_name == "Alice C. Harris Intermediate - Box Elder District" ~ "Alice C. Harris Intermediate",
                                                     place_name == "Century Elementary School - Box Elder District" ~ "Century Elementary",
                                                     place_name == "Sunrise High Elementary School - Box Elder District" ~ "Sunrise High School",
                                                     place_name == "Discovery Elementary School - Box Elder District" ~ "Discovery Elementary",
                                                     place_name == "Fielding Elementary School - Box Elder District" ~ "Fielding Elementary",
                                                     place_name == "Foothill Elementary School - Box Elder District" ~ "Foothill Elementary",
                                                     place_name == "Garland Elementary School - Box Elder District" ~ "Garland Elementary",
                                                     place_name == "Grouse Creek Elementary School - Box Elder District" ~ "Grouse Creek Elementary",
                                                     place_name == "Lake View Elementary School - Box Elder District" ~ "Lake View Elementary",
                                                     place_name == "McKinley Elementary School - Box Elder District" ~ "McKinley Elementary",
                                                     place_name == "Mountain View Elementary School - Box Elder District" ~ "Mountain View Elementary",
                                                     place_name == "North Park Elementary School - Box Elder District" ~ "North Park Elementary (Tremonton)",
                                                     place_name == "Park Valley Elementary School - Box Elder District" ~ "Park Valley Elementary",
                                                     place_name == "Snowville Elementary School - Box Elder District" ~ "Snowville Elementary",
                                                     place_name == "Three Mile Creek Elementary School - Box Elder District" ~ "Three Mile Creek Elementary",
                                                     place_name == "Willard Elementary School - Box Elder District" ~ "Willard Elementary",
                                                     #Cache Schools
                                                     place_name == "Cache Alternative High - Cache District" ~ "Cache Alternative High School",
                                                     place_name == "Green Canyon High School - Cache District" ~ "Green Canyon High School",
                                                     place_name == "Mountain Crest High - Cache District" ~ "Mountain Crest High School",
                                                     place_name == "Ridgeline High School - Cache District" ~ "Ridgeline High School",
                                                     place_name == "Sky View High - Cache District" ~ "Sky View High School",
                                                     place_name == "Birch Creek Elementary School - Cache District" ~ "Birch Creek Elementary",
                                                     place_name == "Canyon Elementary School - Cache District" ~ "Canyon Elementary",
                                                     place_name == "Cedar Ridge Elementary School - Cache District" ~ "Cedar Ridge Elementary",
                                                     place_name == "Greenville Elementary School - Cache District" ~ "Greenville Elementary",
                                                     place_name == "Heritage Elementary School - Cache District" ~ "Heritage Elementary",
                                                     place_name == "Lewiston Elementary School - Cache District" ~ "Lewiston Elementary",
                                                     place_name == "Lincoln Elementary School - Cache District" ~ "Lincoln Elementary",
                                                     place_name == "Millville Elementary School - Cache District" ~ "Millville Elementary",
                                                     place_name == "Mountainside Elementary School - Cache District" ~ "Mountainside Elementary",
                                                     place_name == "Nibley Elementary School - Cache District" ~ "Nibley Elementary",
                                                     place_name == "North Cache Middle School - Cache District" ~ "North Cache Middle School",
                                                     place_name == "North Park Elementary School - Cache District" ~ "North Park Elementary (North Logan)",
                                                     place_name == "Providence Elementary School - Cache District" ~ "Providence Elementary",
                                                     place_name == "River Heights Elementary School - Cache District" ~ "River Heights Elementary",
                                                     place_name == "South Cache Middle School - Cache District" ~ "South Cache Middle School",
                                                     place_name == "Spring Creek Middle School - Cache District" ~ "Spring Creek Middle School",
                                                     place_name == "Summit Elementary School - Cache District" ~ "Summit Elementary",
                                                     place_name == "Sunrise Elementary School - Cache District" ~ "Sunrise Elementary",
                                                     place_name == "Wellsville Elementary School - Cache District" ~ "Wellsville Elementary",
                                                     place_name == "White Pine Elementary School - Cache District" ~ "White Pine Elementary",
                                                     #Logan Schools
                                                     place_name == "Logan High - Logan City District" ~ "Logan High School",
                                                     place_name == "Mount Logan Middle - Logan City District" ~ "Mount Logan Middle School",
                                                     place_name == "Adams Elementary School - Logan City District" ~ "Adams Elementary",
                                                     place_name == "Bridger Elementary School - Logan City District" ~ "Bridger Elementary",
                                                     place_name == "Ellis Elementary School - Logan City District" ~ "Ellis Elementary",
                                                     place_name == "Hillcrest Elementary School - Logan City District" ~ "Hillcrest Elementary",
                                                     place_name == "Wilson Elementary School - Logan City District" ~ "Wilson Elementary",
                                                     place_name == "Woodruff Elementary School - Logan City District" ~ "Woodruff Elementary",
                                                     #Rich Schools
                                                     place_name == "Rich High - Rich District" ~ "Rich High School",
                                                     place_name == "Rich Middle School - Rich District" ~ "Rich Middle School",
                                                     place_name == "North Rich Elementary School - Rich District" ~ "North Rich Elementary",
                                                     place_name == "South Rich Elementary School - Rich District" ~ "South Rich Elementary",
                                                     #Charter/Other Schools, only change Thomas Edison North since other schools can keep their place_name value
                                                     place_name == "Thomas Edison" ~ "Thomas Edison North",
                                                     place_name == "Thomas Edison Charter Schools South" ~ "Thomas Edison South",
                                                     place_name == "Thomas Edison - South - Thomas Edison" ~ "Thomas Edison South",
                                                     #If none of the above, school name is the place_name value (should only apply for the other charter schools)
                                                     TRUE ~ "as.character"(place_name))) %>% 
                           group_by(patient_event_onset_date, School) %>% 
                           rename(Date = patient_event_onset_date) %>% 
                           summarize(`New Cases` = n()), by = c("Date", "School")) %>% 
  mutate(`New Cases` = replace_na(`New Cases`, 0)) %>% group_by(School) %>% 
  mutate(`Total Cases` = cumsum(`New Cases`))  

# Creates Active Cases (defined as ten days past onset date), 
k12_grouped <- left_join(k12_grouped, k12 %>% 
                           mutate(recovered_date = patient_event_onset_date + 10) %>% 
                           rename(Date = recovered_date) %>% 
                           mutate(School = case_when(place_name == "Adele C. Young Intermediate - Box Elder District" ~ "Adele C. Young Intermediate", 
                                                     place_name == "Bear River High - Box Elder District" ~ "Bear River High School",
                                                     place_name == "Bear River Middle - Box Elder District" ~ "Bear River Middle School",
                                                     place_name == "Box Elder High - Box Elder District" ~ "Box Elder High School",
                                                     place_name == "Box Elder Middle - Box Elder District" ~ "Box Elder Middle School",
                                                     place_name == "Alice C. Harris Intermediate - Box Elder District" ~ "Alice C. Harris Intermediate",
                                                     place_name == "Century Elementary School - Box Elder District" ~ "Century Elementary",
                                                     place_name == "Sunrise High Elementary School - Box Elder District" ~ "Sunrise High School",
                                                     place_name == "Discovery Elementary School - Box Elder District" ~ "Discovery Elementary",
                                                     place_name == "Fielding Elementary School - Box Elder District" ~ "Fielding Elementary",
                                                     place_name == "Foothill Elementary School - Box Elder District" ~ "Foothill Elementary",
                                                     place_name == "Garland Elementary School - Box Elder District" ~ "Garland Elementary",
                                                     place_name == "Grouse Creek Elementary School - Box Elder District" ~ "Grouse Creek Elementary",
                                                     place_name == "Lake View Elementary School - Box Elder District" ~ "Lake View Elementary",
                                                     place_name == "McKinley Elementary School - Box Elder District" ~ "McKinley Elementary",
                                                     place_name == "Mountain View Elementary School - Box Elder District" ~ "Mountain View Elementary",
                                                     place_name == "North Park Elementary School - Box Elder District" ~ "North Park Elementary (Tremonton)",
                                                     place_name == "Park Valley Elementary School - Box Elder District" ~ "Park Valley Elementary",
                                                     place_name == "Snowville Elementary School - Box Elder District" ~ "Snowville Elementary",
                                                     place_name == "Three Mile Creek Elementary School - Box Elder District" ~ "Three Mile Creek Elementary",
                                                     place_name == "Willard Elementary School - Box Elder District" ~ "Willard Elementary",
                                                     #Cache Schools
                                                     place_name == "Cache Alternative High - Cache District" ~ "Cache Alternative High School",
                                                     place_name == "Green Canyon High School - Cache District" ~ "Green Canyon High School",
                                                     place_name == "Mountain Crest High - Cache District" ~ "Mountain Crest High School",
                                                     place_name == "Ridgeline High School - Cache District" ~ "Ridgeline High School",
                                                     place_name == "Sky View High - Cache District" ~ "Sky View High School",
                                                     place_name == "Birch Creek Elementary School - Cache District" ~ "Birch Creek Elementary",
                                                     place_name == "Canyon Elementary School - Cache District" ~ "Canyon Elementary",
                                                     place_name == "Cedar Ridge Elementary School - Cache District" ~ "Cedar Ridge Elementary",
                                                     place_name == "Greenville Elementary School - Cache District" ~ "Greenville Elementary",
                                                     place_name == "Heritage Elementary School - Cache District" ~ "Heritage Elementary",
                                                     place_name == "Lewiston Elementary School - Cache District" ~ "Lewiston Elementary",
                                                     place_name == "Lincoln Elementary School - Cache District" ~ "Lincoln Elementary",
                                                     place_name == "Millville Elementary School - Cache District" ~ "Millville Elementary",
                                                     place_name == "Mountainside Elementary School - Cache District" ~ "Mountainside Elementary",
                                                     place_name == "Nibley Elementary School - Cache District" ~ "Nibley Elementary",
                                                     place_name == "North Cache Middle School - Cache District" ~ "North Cache Middle School",
                                                     place_name == "North Park Elementary School - Cache District" ~ "North Park Elementary (North Logan)",
                                                     place_name == "Providence Elementary School - Cache District" ~ "Providence Elementary",
                                                     place_name == "River Heights Elementary School - Cache District" ~ "River Heights Elementary",
                                                     place_name == "South Cache Middle School - Cache District" ~ "South Cache Middle School",
                                                     place_name == "Spring Creek Middle School - Cache District" ~ "Spring Creek Middle School",
                                                     place_name == "Summit Elementary School - Cache District" ~ "Summit Elementary",
                                                     place_name == "Sunrise Elementary School - Cache District" ~ "Sunrise Elementary",
                                                     place_name == "Wellsville Elementary School - Cache District" ~ "Wellsville Elementary",
                                                     place_name == "White Pine Elementary School - Cache District" ~ "White Pine Elementary",
                                                     #Logan Schools
                                                     place_name == "Logan High - Logan City District" ~ "Logan High School",
                                                     place_name == "Mount Logan Middle - Logan City District" ~ "Mount Logan Middle School",
                                                     place_name == "Adams Elementary School - Logan City District" ~ "Adams Elementary",
                                                     place_name == "Bridger Elementary School - Logan City District" ~ "Bridger Elementary",
                                                     place_name == "Ellis Elementary School - Logan City District" ~ "Ellis Elementary",
                                                     place_name == "Hillcrest Elementary School - Logan City District" ~ "Hillcrest Elementary",
                                                     place_name == "Wilson Elementary School - Logan City District" ~ "Wilson Elementary",
                                                     place_name == "Woodruff Elementary School - Logan City District" ~ "Woodruff Elementary",
                                                     #Rich Schools
                                                     place_name == "Rich High - Rich District" ~ "Rich High School",
                                                     place_name == "Rich Middle School - Rich District" ~ "Rich Middle School",
                                                     place_name == "North Rich Elementary School - Rich District" ~ "North Rich Elementary",
                                                     place_name == "South Rich Elementary School - Rich District" ~ "South Rich Elementary",
                                                     #Charter/Other Schools, only change Thomas Edison North since other schools can keep their place_name value
                                                     place_name == "Thomas Edison" ~ "Thomas Edison North",
                                                     place_name == "Thomas Edison Charter Schools South" ~ "Thomas Edison South",
                                                     place_name == "Thomas Edison - South - Thomas Edison" ~ "Thomas Edison South",
                                                     #If none of the above, school name is the place_name value (should only apply for the other charter schools)
                                                     TRUE ~ "as.character"(place_name))) %>%
                           group_by(Date, School) %>% 
                           summarize(n = n()), by = c("Date", "School")) %>% 
  mutate(n = replace_na(n, 0)) %>% group_by(School) %>% mutate(Recovered = cumsum(n), n = NULL, 
                                                               `Active Cases` = `Total Cases` - Recovered) %>% 
  mutate(Five_day_average = round(SMA(`New Cases`, 5),1)) %>% 
  mutate(Incidence.per.1000 = round((Five_day_average)*1000/Enrollment,2)) %>% 
  mutate(ratio = round(Incidence.per.1000/lag(Incidence.per.1000,5),2)) %>% 
  mutate(incidence_per_capita_five_day_decrease = 1 - ratio)

#Add Variables for School transmitted cases
k12_grouped <- left_join(k12_grouped, k12trans %>%
                           mutate(School = case_when(place_name == "Adele C. Young Intermediate - Box Elder District" ~ "Adele C. Young Intermediate", 
                                                     place_name == "Bear River High - Box Elder District" ~ "Bear River High School",
                                                     place_name == "Bear River Middle - Box Elder District" ~ "Bear River Middle School",
                                                     place_name == "Box Elder High - Box Elder District" ~ "Box Elder High School",
                                                     place_name == "Box Elder Middle - Box Elder District" ~ "Box Elder Middle School",
                                                     place_name == "Alice C. Harris Intermediate - Box Elder District" ~ "Alice C. Harris Intermediate",
                                                     place_name == "Century Elementary School - Box Elder District" ~ "Century Elementary",
                                                     place_name == "Sunrise High Elementary School - Box Elder District" ~ "Sunrise High School",
                                                     place_name == "Discovery Elementary School - Box Elder District" ~ "Discovery Elementary",
                                                     place_name == "Fielding Elementary School - Box Elder District" ~ "Fielding Elementary",
                                                     place_name == "Foothill Elementary School - Box Elder District" ~ "Foothill Elementary",
                                                     place_name == "Garland Elementary School - Box Elder District" ~ "Garland Elementary",
                                                     place_name == "Grouse Creek Elementary School - Box Elder District" ~ "Grouse Creek Elementary",
                                                     place_name == "Lake View Elementary School - Box Elder District" ~ "Lake View Elementary",
                                                     place_name == "McKinley Elementary School - Box Elder District" ~ "McKinley Elementary",
                                                     place_name == "Mountain View Elementary School - Box Elder District" ~ "Mountain View Elementary",
                                                     place_name == "North Park Elementary School - Box Elder District" ~ "North Park Elementary (Tremonton)",
                                                     place_name == "Park Valley Elementary School - Box Elder District" ~ "Park Valley Elementary",
                                                     place_name == "Snowville Elementary School - Box Elder District" ~ "Snowville Elementary",
                                                     place_name == "Three Mile Creek Elementary School - Box Elder District" ~ "Three Mile Creek Elementary",
                                                     place_name == "Willard Elementary School - Box Elder District" ~ "Willard Elementary",
                                                     #Cache Schools
                                                     place_name == "Cache Alternative High - Cache District" ~ "Cache Alternative High School",
                                                     place_name == "Green Canyon High School - Cache District" ~ "Green Canyon High School",
                                                     place_name == "Mountain Crest High - Cache District" ~ "Mountain Crest High School",
                                                     place_name == "Ridgeline High School - Cache District" ~ "Ridgeline High School",
                                                     place_name == "Sky View High - Cache District" ~ "Sky View High School",
                                                     place_name == "Birch Creek Elementary School - Cache District" ~ "Birch Creek Elementary",
                                                     place_name == "Canyon Elementary School - Cache District" ~ "Canyon Elementary",
                                                     place_name == "Cedar Ridge Elementary School - Cache District" ~ "Cedar Ridge Elementary",
                                                     place_name == "Greenville Elementary School - Cache District" ~ "Greenville Elementary",
                                                     place_name == "Heritage Elementary School - Cache District" ~ "Heritage Elementary",
                                                     place_name == "Lewiston Elementary School - Cache District" ~ "Lewiston Elementary",
                                                     place_name == "Lincoln Elementary School - Cache District" ~ "Lincoln Elementary",
                                                     place_name == "Millville Elementary School - Cache District" ~ "Millville Elementary",
                                                     place_name == "Mountainside Elementary School - Cache District" ~ "Mountainside Elementary",
                                                     place_name == "Nibley Elementary School - Cache District" ~ "Nibley Elementary",
                                                     place_name == "North Cache Middle School - Cache District" ~ "North Cache Middle School",
                                                     place_name == "North Park Elementary School - Cache District" ~ "North Park Elementary (North Logan)",
                                                     place_name == "Providence Elementary School - Cache District" ~ "Providence Elementary",
                                                     place_name == "River Heights Elementary School - Cache District" ~ "River Heights Elementary",
                                                     place_name == "South Cache Middle School - Cache District" ~ "South Cache Middle School",
                                                     place_name == "Spring Creek Middle School - Cache District" ~ "Spring Creek Middle School",
                                                     place_name == "Summit Elementary School - Cache District" ~ "Summit Elementary",
                                                     place_name == "Sunrise Elementary School - Cache District" ~ "Sunrise Elementary",
                                                     place_name == "Wellsville Elementary School - Cache District" ~ "Wellsville Elementary",
                                                     place_name == "White Pine Elementary School - Cache District" ~ "White Pine Elementary",
                                                     #Logan Schools
                                                     place_name == "Logan High - Logan City District" ~ "Logan High School",
                                                     place_name == "Mount Logan Middle - Logan City District" ~ "Mount Logan Middle School",
                                                     place_name == "Adams Elementary School - Logan City District" ~ "Adams Elementary",
                                                     place_name == "Bridger Elementary School - Logan City District" ~ "Bridger Elementary",
                                                     place_name == "Ellis Elementary School - Logan City District" ~ "Ellis Elementary",
                                                     place_name == "Hillcrest Elementary School - Logan City District" ~ "Hillcrest Elementary",
                                                     place_name == "Wilson Elementary School - Logan City District" ~ "Wilson Elementary",
                                                     place_name == "Woodruff Elementary School - Logan City District" ~ "Woodruff Elementary",
                                                     #Rich Schools
                                                     place_name == "Rich High - Rich District" ~ "Rich High School",
                                                     place_name == "Rich Middle School - Rich District" ~ "Rich Middle School",
                                                     place_name == "North Rich Elementary School - Rich District" ~ "North Rich Elementary",
                                                     place_name == "South Rich Elementary School - Rich District" ~ "South Rich Elementary",
                                                     #Charter/Other Schools, only change Thomas Edison North since other schools can keep their place_name value
                                                     place_name == "Thomas Edison" ~ "Thomas Edison North",
                                                     place_name == "Thomas Edison Charter Schools South" ~ "Thomas Edison South",
                                                     place_name == "Thomas Edison - South - Thomas Edison" ~ "Thomas Edison South",
                                                     #If none of the above, school name is the place_name value (should only apply for the other charter schools)
                                                     TRUE ~ "as.character"(place_name))) %>% 
                           group_by(patient_event_onset_date, School) %>% 
                           rename(Date = patient_event_onset_date) %>% 
                           summarize(`New School Transmission` = n()), by = c("Date", "School")) %>% 
  mutate(`New School Transmission` = replace_na(`New School Transmission`, 0)) %>% group_by(School) %>% 
  mutate(`Total School-Transmitted Cases` = cumsum(`New School Transmission`))

k12$patient_event_onset_date <- as.Date(k12$patient_event_onset_date)

# Creates Percent of Cases Transmitted by 
k12_grouped <- left_join(k12_grouped, k12trans %>% 
                           mutate(recovered_date = patient_event_onset_date + 10) %>% 
                           rename(Date = recovered_date) %>% 
                           mutate(School = case_when(place_name == "Adele C. Young Intermediate - Box Elder District" ~ "Adele C. Young Intermediate", 
                                                     place_name == "Bear River High - Box Elder District" ~ "Bear River High School",
                                                     place_name == "Bear River Middle - Box Elder District" ~ "Bear River Middle School",
                                                     place_name == "Box Elder High - Box Elder District" ~ "Box Elder High School",
                                                     place_name == "Box Elder Middle - Box Elder District" ~ "Box Elder Middle School",
                                                     place_name == "Alice C. Harris Intermediate - Box Elder District" ~ "Alice C. Harris Intermediate",
                                                     place_name == "Century Elementary School - Box Elder District" ~ "Century Elementary",
                                                     place_name == "Sunrise High Elementary School - Box Elder District" ~ "Sunrise High School",
                                                     place_name == "Discovery Elementary School - Box Elder District" ~ "Discovery Elementary",
                                                     place_name == "Fielding Elementary School - Box Elder District" ~ "Fielding Elementary",
                                                     place_name == "Foothill Elementary School - Box Elder District" ~ "Foothill Elementary",
                                                     place_name == "Garland Elementary School - Box Elder District" ~ "Garland Elementary",
                                                     place_name == "Grouse Creek Elementary School - Box Elder District" ~ "Grouse Creek Elementary",
                                                     place_name == "Lake View Elementary School - Box Elder District" ~ "Lake View Elementary",
                                                     place_name == "McKinley Elementary School - Box Elder District" ~ "McKinley Elementary",
                                                     place_name == "Mountain View Elementary School - Box Elder District" ~ "Mountain View Elementary",
                                                     place_name == "North Park Elementary School - Box Elder District" ~ "North Park Elementary (Tremonton)",
                                                     place_name == "Park Valley Elementary School - Box Elder District" ~ "Park Valley Elementary",
                                                     place_name == "Snowville Elementary School - Box Elder District" ~ "Snowville Elementary",
                                                     place_name == "Three Mile Creek Elementary School - Box Elder District" ~ "Three Mile Creek Elementary",
                                                     place_name == "Willard Elementary School - Box Elder District" ~ "Willard Elementary",
                                                     #Cache Schools
                                                     place_name == "Cache Alternative High - Cache District" ~ "Cache Alternative High School",
                                                     place_name == "Green Canyon High School - Cache District" ~ "Green Canyon High School",
                                                     place_name == "Mountain Crest High - Cache District" ~ "Mountain Crest High School",
                                                     place_name == "Ridgeline High School - Cache District" ~ "Ridgeline High School",
                                                     place_name == "Sky View High - Cache District" ~ "Sky View High School",
                                                     place_name == "Birch Creek Elementary School - Cache District" ~ "Birch Creek Elementary",
                                                     place_name == "Canyon Elementary School - Cache District" ~ "Canyon Elementary",
                                                     place_name == "Cedar Ridge Elementary School - Cache District" ~ "Cedar Ridge Elementary",
                                                     place_name == "Greenville Elementary School - Cache District" ~ "Greenville Elementary",
                                                     place_name == "Heritage Elementary School - Cache District" ~ "Heritage Elementary",
                                                     place_name == "Lewiston Elementary School - Cache District" ~ "Lewiston Elementary",
                                                     place_name == "Lincoln Elementary School - Cache District" ~ "Lincoln Elementary",
                                                     place_name == "Millville Elementary School - Cache District" ~ "Millville Elementary",
                                                     place_name == "Mountainside Elementary School - Cache District" ~ "Mountainside Elementary",
                                                     place_name == "Nibley Elementary School - Cache District" ~ "Nibley Elementary",
                                                     place_name == "North Cache Middle School - Cache District" ~ "North Cache Middle School",
                                                     place_name == "North Park Elementary School - Cache District" ~ "North Park Elementary (North Logan)",
                                                     place_name == "Providence Elementary School - Cache District" ~ "Providence Elementary",
                                                     place_name == "River Heights Elementary School - Cache District" ~ "River Heights Elementary",
                                                     place_name == "South Cache Middle School - Cache District" ~ "South Cache Middle School",
                                                     place_name == "Spring Creek Middle School - Cache District" ~ "Spring Creek Middle School",
                                                     place_name == "Summit Elementary School - Cache District" ~ "Summit Elementary",
                                                     place_name == "Sunrise Elementary School - Cache District" ~ "Sunrise Elementary",
                                                     place_name == "Wellsville Elementary School - Cache District" ~ "Wellsville Elementary",
                                                     place_name == "White Pine Elementary School - Cache District" ~ "White Pine Elementary",
                                                     #Logan Schools
                                                     place_name == "Logan High - Logan City District" ~ "Logan High School",
                                                     place_name == "Mount Logan Middle - Logan City District" ~ "Mount Logan Middle School",
                                                     place_name == "Adams Elementary School - Logan City District" ~ "Adams Elementary",
                                                     place_name == "Bridger Elementary School - Logan City District" ~ "Bridger Elementary",
                                                     place_name == "Ellis Elementary School - Logan City District" ~ "Ellis Elementary",
                                                     place_name == "Hillcrest Elementary School - Logan City District" ~ "Hillcrest Elementary",
                                                     place_name == "Wilson Elementary School - Logan City District" ~ "Wilson Elementary",
                                                     place_name == "Woodruff Elementary School - Logan City District" ~ "Woodruff Elementary",
                                                     #Rich Schools
                                                     place_name == "Rich High - Rich District" ~ "Rich High School",
                                                     place_name == "Rich Middle School - Rich District" ~ "Rich Middle School",
                                                     place_name == "North Rich Elementary School - Rich District" ~ "North Rich Elementary",
                                                     place_name == "South Rich Elementary School - Rich District" ~ "South Rich Elementary",
                                                     #Charter/Other Schools, only change Thomas Edison North since other schools can keep their place_name value
                                                     place_name == "Thomas Edison" ~ "Thomas Edison North",
                                                     place_name == "Thomas Edison Charter Schools South" ~ "Thomas Edison South",
                                                     place_name == "Thomas Edison - South - Thomas Edison" ~ "Thomas Edison South",
                                                     #If none of the above, school name is the place_name value (should only apply for the other charter schools)
                                                     TRUE ~ "as.character"(place_name))) %>%
                           group_by(Date, School) %>% 
                           summarize(n = n()), by = c("Date", "School")) %>% 
  mutate(n = replace_na(n, 0)) %>% group_by(School) %>% mutate(ST_Recovered = cumsum(n), n = NULL, 
                                                               `Active Cases from School Transmission` = `Total School-Transmitted Cases` - ST_Recovered)



#Create Reactable for each school district and BRHD as a whole

######
#BESD#
######
#Filter k12_grouped to besd
k12_besd <- k12_grouped %>% 
  filter(District == "Box Elder School District")
#Create BESD daily data
besd_grouped <- left_join(tibble(Date = seq.Date(as.Date("2021-08-18"),todays_date,by=1)),
                          group_by(k12, covid_case_date) %>% 
                            filter(District == "Box Elder School District", corona_pui_col_school_univ_type == "Student") %>% 
                            rename(Date = covid_case_date) %>% summarize(`New Student Cases` = n()),
                          by = "Date") %>% 
  mutate(`New Student Cases` = replace_na(`New Student Cases`, 0), `Total Student Cases` = cumsum(`New Student Cases`)) %>% 
  mutate(School = "Box Elder School District", Enrollment = 12800)

besd_grouped <- left_join(besd_grouped,
                          group_by(k12, patient_event_onset_date) %>% 
                            filter(District == "Box Elder School District") %>% 
                            rename(Date = patient_event_onset_date) %>% 
                            summarize(`New Cases` = n()),
                          by = "Date") %>% 
  mutate(`New Cases` = replace_na(`New Cases`, 0), `Total Cases` = cumsum(`New Cases`))

besd_grouped <- left_join(besd_grouped,
                          group_by(k12trans, patient_event_onset_date) %>% 
                            filter(District == "Box Elder School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                            rename(Date = patient_event_onset_date) %>% 
                            summarize(`New School Transmission` = n()),
                          by = "Date") %>% 
  mutate(`New School Transmission` = replace_na(`New School Transmission`, 0), `Total School-Transmitted Cases` = cumsum(`New School Transmission`))

besd_grouped <- left_join(besd_grouped,
                          filter(k12stud, District == "Box Elder School District") %>% 
                            mutate(threshold_date = covid_case_date + 7) %>% 
                            count(Date = threshold_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Students after 7` = cumsum(n), n = NULL,
         `Current Threshold Cases` = `Total Student Cases` - `Students after 7`,
         `Percent of Enrollment` = round((`Current Threshold Cases`/Enrollment)*100,2))

besd_grouped <- left_join(besd_grouped,
                          filter(k12, District == "Box Elder School District") %>% 
                            mutate(recovered_date = patient_event_onset_date + 10) %>% 
                            count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Recovered` = cumsum(n), n = NULL,
         `Active Cases` = `Total Cases` - `Recovered`)

besd_grouped <- left_join(besd_grouped,
                          filter(k12, District == "Box Elder School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                            mutate(recovered_date = patient_event_onset_date + 10) %>% 
                            count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `ST_Recovered` = cumsum(n), n = NULL,
         `Active Cases from School Transmission` = `Total School-Transmitted Cases` - `ST_Recovered`)

#Create BESD table for reactable
besd_react <- bind_rows(k12_besd, besd_grouped) %>% 
  filter(Date == max(besd_grouped$Date)) %>% 
  dplyr::select(School, `New Student Cases`, `Total Cases`, `Enrollment`, `Current Threshold Cases`, `Percent of Enrollment`)

######
#CCSD#
######
#Filter k12_grouped to besd
k12_ccsd <- k12_grouped %>% 
  filter(District == "Cache County School District")
#Create CCSD daily data
ccsd_grouped <- left_join(tibble(Date = seq.Date(as.Date("2021-08-18"),todays_date,by=1)),
                          group_by(k12, covid_case_date) %>% 
                            filter(District == "Cache County School District", corona_pui_col_school_univ_type == "Student") %>% 
                            rename(Date = covid_case_date) %>% summarize(`New Student Cases` = n()),
                          by = "Date") %>% 
  mutate(`New Student Cases` = replace_na(`New Student Cases`, 0), `Total Student Cases` = cumsum(`New Student Cases`)) %>% 
  mutate(School = "Cache County School District", Enrollment = 18809)

ccsd_grouped <- left_join(ccsd_grouped,
                          group_by(k12, patient_event_onset_date) %>% 
                            filter(District == "Cache County School District") %>% 
                            rename(Date = patient_event_onset_date) %>% 
                            summarize(`New Cases` = n()),
                          by = "Date") %>% 
  mutate(`New Cases` = replace_na(`New Cases`, 0), `Total Cases` = cumsum(`New Cases`))

ccsd_grouped <- left_join(ccsd_grouped,
                          group_by(k12trans, patient_event_onset_date) %>% 
                            filter(District == "Cache County School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                            rename(Date = patient_event_onset_date) %>% 
                            summarize(`New School Transmission` = n()),
                          by = "Date") %>% 
  mutate(`New School Transmission` = replace_na(`New School Transmission`, 0), `Total School-Transmitted Cases` = cumsum(`New School Transmission`))

ccsd_grouped <- left_join(ccsd_grouped,
                          filter(k12stud, District == "Cache County School District") %>% 
                            mutate(threshold_date = covid_case_date + 7) %>% 
                            count(Date = threshold_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Students after 7` = cumsum(n), n = NULL,
         `Current Threshold Cases` = `Total Student Cases` - `Students after 7`,
         `Percent of Enrollment` = round((`Current Threshold Cases`/Enrollment)*100,2))

ccsd_grouped <- left_join(ccsd_grouped,
                          filter(k12, District == "Cache County School District") %>% 
                            mutate(recovered_date = patient_event_onset_date + 10) %>% 
                            count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Recovered` = cumsum(n), n = NULL,
         `Active Cases` = `Total Cases` - `Recovered`)

ccsd_grouped <- left_join(ccsd_grouped,
                          filter(k12, District == "Cache County School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                            mutate(recovered_date = patient_event_onset_date + 10) %>% 
                            count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `ST_Recovered` = cumsum(n), n = NULL,
         `Active Cases from School Transmission` = `Total School-Transmitted Cases` - `ST_Recovered`)

#Create CCSD table for reactable
ccsd_react <- bind_rows(k12_ccsd, ccsd_grouped) %>% 
  filter(Date == max(ccsd_grouped$Date)) %>% 
  dplyr::select(School, `New Student Cases`, `Total Cases`, `Enrollment`, `Current Threshold Cases`, `Percent of Enrollment`)

######
#LCSD#
######
#Filter k12_grouped to besd
k12_lcsd <- k12_grouped %>% 
  filter(District == "Logan City School District")
#Create lcsd daily data
lcsd_grouped <- left_join(tibble(Date = seq.Date(as.Date("2021-08-18"),todays_date,by=1)),
                          group_by(k12, covid_case_date) %>% 
                            filter(District == "Logan City School District", corona_pui_col_school_univ_type == "Student") %>% 
                            rename(Date = covid_case_date) %>% summarize(`New Student Cases` = n()),
                          by = "Date") %>% 
  mutate(`New Student Cases` = replace_na(`New Student Cases`, 0), `Total Student Cases` = cumsum(`New Student Cases`)) %>% 
  mutate(School = "Logan City School District", Enrollment = 5199)

lcsd_grouped <- left_join(lcsd_grouped,
                          group_by(k12, patient_event_onset_date) %>% 
                            filter(District == "Logan City School District") %>% 
                            rename(Date = patient_event_onset_date) %>% 
                            summarize(`New Cases` = n()),
                          by = "Date") %>% 
  mutate(`New Cases` = replace_na(`New Cases`, 0), `Total Cases` = cumsum(`New Cases`))

lcsd_grouped <- left_join(lcsd_grouped,
                          group_by(k12trans, patient_event_onset_date) %>% 
                            filter(District == "Logan City School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                            rename(Date = patient_event_onset_date) %>% 
                            summarize(`New School Transmission` = n()),
                          by = "Date") %>% 
  mutate(`New School Transmission` = replace_na(`New School Transmission`, 0), `Total School-Transmitted Cases` = cumsum(`New School Transmission`))

lcsd_grouped <- left_join(lcsd_grouped,
                          filter(k12stud, District == "Logan City School District") %>% 
                            mutate(threshold_date = covid_case_date + 7) %>% 
                            count(Date = threshold_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Students after 7` = cumsum(n), n = NULL,
         `Current Threshold Cases` = `Total Student Cases` - `Students after 7`,
         `Percent of Enrollment` = round((`Current Threshold Cases`/Enrollment)*100,2))

lcsd_grouped <- left_join(lcsd_grouped,
                          filter(k12, District == "Logan City School District") %>% 
                            mutate(recovered_date = patient_event_onset_date + 10) %>% 
                            count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Recovered` = cumsum(n), n = NULL,
         `Active Cases` = `Total Cases` - `Recovered`)

lcsd_grouped <- left_join(lcsd_grouped,
                          filter(k12, District == "Logan City School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                            mutate(recovered_date = patient_event_onset_date + 10) %>% 
                            count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `ST_Recovered` = cumsum(n), n = NULL,
         `Active Cases from School Transmission` = `Total School-Transmitted Cases` - `ST_Recovered`)

#Create lcsd table for reactable
lcsd_react <- bind_rows(k12_lcsd, lcsd_grouped) %>% 
  filter(Date == max(lcsd_grouped$Date)) %>% 
  dplyr::select(School, `New Student Cases`, `Total Cases`, `Enrollment`, `Current Threshold Cases`, `Percent of Enrollment`)

#####
#RSD#
#####
#Filter k12_grouped to besd
k12_rsd <- k12_grouped %>% 
  filter(District == "Rich School District")
#Create rsd daily data
rsd_grouped <- left_join(tibble(Date = seq.Date(as.Date("2021-08-18"),todays_date,by=1)),
                         group_by(k12, covid_case_date) %>% 
                           filter(District == "Rich School District", corona_pui_col_school_univ_type == "Student") %>% 
                           rename(Date = covid_case_date) %>% summarize(`New Student Cases` = n()),
                         by = "Date") %>% 
  mutate(`New Student Cases` = replace_na(`New Student Cases`, 0), `Total Student Cases` = cumsum(`New Student Cases`)) %>% 
  mutate(School = "Rich School District", Enrollment = 511)

rsd_grouped <- left_join(rsd_grouped,
                         group_by(k12, patient_event_onset_date) %>% 
                           filter(District == "Rich School District") %>% 
                           rename(Date = patient_event_onset_date) %>% 
                           summarize(`New Cases` = n()),
                         by = "Date") %>% 
  mutate(`New Cases` = replace_na(`New Cases`, 0), `Total Cases` = cumsum(`New Cases`))

rsd_grouped <- left_join(rsd_grouped,
                         group_by(k12trans, patient_event_onset_date) %>% 
                           filter(District == "Rich School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                           rename(Date = patient_event_onset_date) %>% 
                           summarize(`New School Transmission` = n()),
                         by = "Date") %>% 
  mutate(`New School Transmission` = replace_na(`New School Transmission`, 0), `Total School-Transmitted Cases` = cumsum(`New School Transmission`))

rsd_grouped <- left_join(rsd_grouped,
                         filter(k12stud, District == "Rich School District") %>% 
                           mutate(threshold_date = covid_case_date + 7) %>% 
                           count(Date = threshold_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Students after 7` = cumsum(n), n = NULL,
         `Current Threshold Cases` = `Total Student Cases` - `Students after 7`,
         `Percent of Enrollment` = round((`Current Threshold Cases`/Enrollment)*100,2))

rsd_grouped <- left_join(rsd_grouped,
                         filter(k12, District == "Rich School District") %>% 
                           mutate(recovered_date = patient_event_onset_date + 10) %>% 
                           count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Recovered` = cumsum(n), n = NULL,
         `Active Cases` = `Total Cases` - `Recovered`)

rsd_grouped <- left_join(rsd_grouped,
                         filter(k12, District == "Rich School District", corona_pui_col_school_acquired_case == "Yes") %>% 
                           mutate(recovered_date = patient_event_onset_date + 10) %>% 
                           count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `ST_Recovered` = cumsum(n), n = NULL,
         `Active Cases from School Transmission` = `Total School-Transmitted Cases` - `ST_Recovered`)

#Create rsd table for reactable
rsd_react <- bind_rows(k12_rsd, rsd_grouped) %>% 
  filter(Date == max(rsd_grouped$Date)) %>% 
  dplyr::select(School, `New Student Cases`, `Total Cases`, `Enrollment`, `Current Threshold Cases`, `Percent of Enrollment`)

###############
#Charter/Other#
###############
#Filter k12_grouped to charter/other schools
k12_co <- k12_grouped %>% 
  filter(District == "Charter/Other Schools")
#Create co daily data
co_grouped <- left_join(tibble(Date = seq.Date(as.Date("2021-08-18"),todays_date,by=1)),
                        group_by(k12, covid_case_date) %>% 
                          filter(District == "Charter/Other Schools", corona_pui_col_school_univ_type == "Student") %>% 
                          rename(Date = covid_case_date) %>% summarize(`New Student Cases` = n()),
                        by = "Date") %>% 
  mutate(`New Student Cases` = replace_na(`New Student Cases`, 0), `Total Student Cases` = cumsum(`New Student Cases`)) %>% 
  mutate(School = "Charter/Other Schools", Enrollment = NA)

co_grouped <- left_join(co_grouped,
                        group_by(k12, patient_event_onset_date) %>% 
                          filter(District == "Charter/Other Schools") %>% 
                          rename(Date = patient_event_onset_date) %>% 
                          summarize(`New Cases` = n()),
                        by = "Date") %>% 
  mutate(`New Cases` = replace_na(`New Cases`, 0), `Total Cases` = cumsum(`New Cases`))

co_grouped <- left_join(co_grouped,
                        group_by(k12trans, patient_event_onset_date) %>% 
                          filter(District == "Charter/Other Schools", corona_pui_col_school_acquired_case == "Yes") %>% 
                          rename(Date = patient_event_onset_date) %>% 
                          summarize(`New School Transmission` = n()),
                        by = "Date") %>% 
  mutate(`New School Transmission` = replace_na(`New School Transmission`, 0), `Total School-Transmitted Cases` = cumsum(`New School Transmission`))

co_grouped <- left_join(co_grouped,
                        filter(k12stud, District == "Charter/Other Schools") %>% 
                          mutate(threshold_date = covid_case_date + 7) %>% 
                          count(Date = threshold_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Students after 7` = cumsum(n), n = NULL,
         `Current Threshold Cases` = `Total Student Cases` - `Students after 7`,
         `Percent of Enrollment` = round((`Current Threshold Cases`/Enrollment)*100,2))

co_grouped <- left_join(co_grouped,
                        filter(k12, District == "Charter/Other Schools") %>% 
                          mutate(recovered_date = patient_event_onset_date + 10) %>% 
                          count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `Recovered` = cumsum(n), n = NULL,
         `Active Cases` = `Total Cases` - `Recovered`)

co_grouped <- left_join(co_grouped,
                        filter(k12, District == "Charter/Other Schools", corona_pui_col_school_acquired_case == "Yes") %>% 
                          mutate(recovered_date = patient_event_onset_date + 10) %>% 
                          count(Date = recovered_date), by = "Date") %>% 
  mutate(n = replace_na(n, 0), `ST_Recovered` = cumsum(n), n = NULL,
         `Active Cases from School Transmission` = `Total School-Transmitted Cases` - `ST_Recovered`)

#Create co table for reactable
co_react <- bind_rows(k12_co, co_grouped) %>% 
  filter(Date == max(co_grouped$Date)) %>% 
  dplyr::select(School, `New Student Cases`, `Total Cases`, `Enrollment`, `Current Threshold Cases`, `Percent of Enrollment`)


table(k12$first_positive_lab)

#################################################################################################
# Find New/Updated Cases #
#################################################################################################
#Load last k12 data set
lastk12 <- read.csv("lastk12.csv", header = T) %>%
  dplyr::select(-1) %>% 
  rename(`Week Of` = Week.Of)

lastk12$first_positive_lab <- as.Date(as.character.Date(lastk12$first_positive_lab))
lastk12$patient_birth_date <- as.Date(lastk12$patient_birth_date)

#Merge old and new k12 complete cases
k12_merged <- merge(lastk12, k12, all = TRUE)

table(k12_merged$first_positive_lab)
class(k12_merged$first_positive_lab)

#Remove duplicates, leaving only new complete k12 cases that are ready to send to school
k12_new <- k12_merged[!(duplicated(k12_merged$patient_record_number) | duplicated(k12_merged$patient_record_number, fromLast = TRUE)), ]


##ONLY WRITE IF THE ABOVE STEP WORKED###
#Write current k12 data set to working directory for future comparisons
write.csv(k12, "lastk12.csv")

#write.csv(lastk12, "lastk12.csv")
write.csv(k12_merged, "k12_merged.csv")
write.csv(k12_new, "k12_new.csv", row.names = F)

class(k12_new$first_positive_lab)
#########################################
# Check for districts/schools to notify #
#########################################
table(k12_new$District)
table(k12_new$place_name)
#Prepare export for each school/district with new cases

k12_new$first_positive_lab <- as.Date(k12_new$first_positive_lab)

#Cache
new_ccsd <- k12_new %>% 
  filter(District == "Cache County School District") %>%
 
 rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 
 
#Box Elder
new_besd <- k12_new %>% 
  filter(District == "Box Elder School District") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Logan
new_lcsd <- k12_new %>% 
  filter(District == "Logan City School District") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Rich High School
new_rhs <- k12_new %>% 
  filter(place_name == "Rich High - Rich District") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Rich Middle School
new_rms <- k12_new %>% 
  filter(place_name == "Rich Middle School - Rich District") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#North Rich Elementary
new_nre <- k12_new %>% 
  filter(place_name == "North Rich Elementary School - Rich District") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

table(k12_new$place_name)

#South Rich Elementary
new_sre <- k12_new %>% 
  filter(place_name == "South Rich Elementary School - Rich District") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 


#Bear River Charter
new_brcs <- k12_new %>% 
  filter(place_name == "Bear River Charter School") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#BATC
new_batc <- k12_new %>% 
  filter(place_name == "Bridgerland Technical College") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#CCID
new_ccid <- k12_new %>% 
  filter(place_name == "The Center for Creativity Innovation and Discovery") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Edith Bowen
new_ebls <- k12_new %>% 
  filter(place_name == "Edith Bowen Laboratory School") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Thomas Edison North
new_ten <- k12_new %>% 
  filter(place_name == "Thomas Edison") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Thomas Edison South
new_tes <- k12_new %>% 
  filter(place_name %in% c("Thomas Edison - South - Thomas Edison", "Thomas Edison Charter Schools South","Thomas Edison Charter School South")) %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Fast Forward
new_ff <- k12_new %>% 
  filter(place_name == "Fast Forward High") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Intech
new_ica <- k12_new %>% 
  filter(place_name == "Intech Collegiate Academy") %>% 
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 

#Promontory
new_psel <- k12_new %>% 
  filter(place_name == "Promontory School of Expeditionary Learning") %>%
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name,  Test.Collection.Date = first_positive_lab,
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details) %>% 
  mutate(Isolate.Through = Test.Collection.Date + 5) %>%
  mutate(Can.Return.On = Test.Collection.Date + 6) %>%
  dplyr::select(First.Name, Last.Name, DOB, School.Type, School.type.detail, School, Test.Collection.Date,  Isolate.Through, Can.Return.On) 


write.csv(new_ccsd, "new_ccsd.csv", row.names = F)
write.csv(new_besd, "new_besd.csv", row.names = F)
write.csv(new_lcsd, "new_lcsd.csv", row.names = F)
write.csv(new_rhs, "new_rhs.csv", row.names = F)
write.csv(new_nre, "new_nre.csv", row.names = F)
write.csv(new_sre, "new_sre.csv", row.names = F)
write.csv(new_rms, "new_rms.csv", row.names = F)
write.csv(new_brcs, "new_brcs.csv", row.names = F) 
write.csv(new_batc, "new_batc.csv", row.names = F)
write.csv(new_ccid, "new_ccid.csv", row.names = F)
write.csv(new_ebls, "new_ebls.csv", row.names = F)
write.csv(new_ten, "new_ten.csv", row.names = F)
write.csv(new_tes, "new_tes.csv", row.names = F)
write.csv(new_ff, "new_ff.csv", row.names = F)
write.csv(new_ica, "new_ica.csv", row.names = F)
write.csv(new_psel, "new_psel.csv", row.names = F)
write.csv(k12_new, "k12_new.csv", row.names = F)

############################################################################
#Each School District by name
fourteen_ccsd <- k12 %>% 
  filter(District == "Cache County School District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)


#Box Elder
fourteen_besd <- k12 %>% 
  filter(District == "Box Elder School District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)


#Logan
fourteen_lcsd <- k12 %>% 
  filter(District == "Logan City School District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)


#Rich High School
fourteen_rhs <- k12 %>% 
  filter(place_name == "Rich High - Rich District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#Rich Middle School
fourteen_rms <- k12 %>% 
  filter(place_name == "Rich Middle School - Rich District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#North Rich Elementary
fourteen_nre <- k12 %>% 
  filter(place_name == "North Rich Elementary School - Rich District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#South Rich Elementary
fourteen_sre <- k12 %>% 
  filter(place_name == "South Rich Elementary School - Rich District") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#Bear River Charter
fourteen_brcs <- k12 %>% 
  filter(place_name == "Bear River Charter School") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#BATC
fourteen_batc <- k12 %>% 
  filter(place_name == "Bridgerland Technical College") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>%  
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#CCID
fourteen_ccid <- k12 %>% 
  filter(place_name == "The Center for Creativity Innovation and Discovery") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)


#Edith Bowen
fourteen_ebls <- k12 %>% 
  filter(place_name == "Edith Bowen Laboratory School") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#Thomas Edison North
fourteen_ten <- k12 %>% 
  filter(place_name == "Thomas Edison") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>%  
  mutate(School = "Thomas Edison North") %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)


#Thomas Edison South
fourteen_tes <- k12 %>% 
  filter(place_name %in% c("Thomas Edison - South - Thomas Edison", "Thomas Edison Charter Schools South")) %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  mutate(School = "Thomas Edison South") %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#Fast Forward
fourteen_ff <- k12 %>% 
  filter(place_name == "Fast Forward High") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>%  
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#Intech
fourteen_ica <- k12 %>% 
  filter(place_name == "Intech Collegiate Academy") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>%  
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)

#Promontory
fourteen_psel <- k12 %>% 
  filter(place_name == "Promontory School of Expeditionary Learning") %>% 
  filter(covid_case_date >= todays_date - 13) %>%  
  rename(First.Name = person_first_name, Last.Name = person_last_name,
         DOB = patient_birth_date, School = place_name, Test.Report.Date = covid_case_date, Onset.Date = patient_event_onset_date, 
         School.Type = corona_pui_col_school_univ_type, School.type.detail = corona_pui_col_school_univ_type_details, Contact.Tracing.Needed = corona_pui_col_school_univ_event, Tracing.Details = corona_pui_col_school_univ_event_details,
         Extracurriculars = corona_pui_col_school_univ_activity, Ext.Other = corona_pui_col_school_univ_activities_details, Bus = corona_pui_col_school_univ_bus_details) %>% 
  mutate(Symptoms = if_else(corona_pui_col_covid_sympt == "Yes", "Yes",
                            if_else(corona_pui_col_covid_sympt == "No", "No", "Unknown"))) %>% 
  mutate(Isolate.Through = Onset.Date + 10) %>%
  mutate(Infectious.Start.Date = Onset.Date - 2) %>% 
  dplyr::select(First.Name, Last.Name, DOB, Test.Report.Date, School.Type, School.type.detail, School, District, Symptoms, Onset.Date, Infectious.Start.Date, Contact.Tracing.Needed,
                Tracing.Details, Extracurriculars, Ext.Other, Bus, Isolate.Through)


write.csv(fourteen_ccsd, "fourteen_ccsd.csv", row.names = F)
write.csv(fourteen_besd, "fourteen_besd.csv", row.names = F)
write.csv(fourteen_lcsd, "fourteen_lcsd.csv", row.names = F)
write.csv(fourteen_rhs, "fourteen_rhs.csv", row.names = F)
write.csv(fourteen_rms, "fourteen_rms.csv", row.names = F)
write.csv(fourteen_nre, "fourteen_nre.csv", row.names = F)
write.csv(fourteen_sre, "fourteen_sre.csv", row.names = F)
write.csv(fourteen_brcs, "fourteen_brcs.csv", row.names = F)
write.csv(fourteen_batc, "fourteen_batc.csv", row.names = F)
write.csv(fourteen_ccid, "fourteen_ccid.csv", row.names = F)
write.csv(fourteen_ebls, "fourteen_ebls.csv", row.names = F)
write.csv(fourteen_ten, "fourteen_ten.csv", row.names = F)
write.csv(fourteen_tes, "fourteen_tes.csv", row.names = F)
write.csv(fourteen_ff, "fourteen_ff.csv", row.names = F)
write.csv(fourteen_ica, "fourteen_ica.csv", row.names = F)
write.csv(fourteen_psel, "fourteen_psel.csv", row.names = F)



#Or
#https://stackoverflow.com/questions/12573456/returning-non-matching-records-from-a-merge

#Other options
#https://stackoverflow.com/questions/63184392/pandas-merge-and-keep-only-non-matching-records
#Anti join
#https://stackoverflow.com/questions/59293061/using-r-merge-to-collect-non-matching-ids

#################################################
#Save Dataframes for App
write.csv(besd_react, "C:/Users/tstoker/Desktop/RWD/School/besd_react.csv")
write.csv(k12_besd, "C:/Users/tstoker/Desktop/RWD/School/k12_besd.csv")
write.csv(ccsd_react, "C:/Users/tstoker/Desktop/RWD/School/ccsd_react.csv")
write.csv(k12_ccsd, "C:/Users/tstoker/Desktop/RWD/School/k12_ccsd.csv")
write.csv(lcsd_react, "C:/Users/tstoker/Desktop/RWD/School/lcsd_react.csv")
write.csv(k12_lcsd, "C:/Users/tstoker/Desktop/RWD/School/k12_lcsd.csv")
write.csv(rsd_react, "C:/Users/tstoker/Desktop/RWD/School/rsd_react.csv")
write.csv(k12_rsd, "C:/Users/tstoker/Desktop/RWD/School/k12_rsd.csv")
write.csv(co_react, "C:/Users/tstoker/Desktop/RWD/School/co_react.csv")
write.csv(k12_co, "C:/Users/tstoker/Desktop/RWD/School/k12_co.csv")

write.csv(besd_grouped, "C:/Users/tstoker/Desktop/RWD/School/besd_grouped.csv")
write.csv(ccsd_grouped, "C:/Users/tstoker/Desktop/RWD/School/ccsd_grouped.csv")
write.csv(lcsd_grouped, "C:/Users/tstoker/Desktop/RWD/School/lcsd_grouped.csv")
write.csv(rsd_grouped, "C:/Users/tstoker/Desktop/RWD/School/rsd_grouped.csv")
write.csv(co_grouped, "C:/Users/tstoker/Desktop/RWD/School/co_grouped.csv")

#write.csv(k12, "C:/Users/tstoker/Desktop/RWD/k12yesterday.csv")




#############################################################################################
#############################################################################
#Search for missing school names
school_missing <- school_cases %>%
  filter(covid_case_date >= '2021-08-30') %>% 
  filter(corona_pui_col_school_univ == "Yes") %>% 
  filter(place_name == "")


#5-17 Box Elder- Not investigated, missing school name
minors_ni_be <- epitraxudoh_merge %>% 
  filter(covid_case_date >= "2021-08-30") %>% 
  filter(patient_age_at_event_in_years <= 17, patient_age_at_event_in_years >= 5) %>% 
  filter(address_at_diagnosis_county == "BE") %>% 
  filter(place_name == "") %>% 
  dplyr::select(patient_record_number, person_last_name, person_first_name, first_positive_lab, patient_birth_date, address_at_diagnosis_county, address_at_diagnosis_city)


#5-17 Cache- Not investigated, missing school name
minors_ni_ca <- epitraxudoh_merge %>% 
  filter(covid_case_date >= "2021-08-18") %>% 
  filter(patient_age_at_event_in_years <= 17, patient_age_at_event_in_years >= 5) %>% 
  filter(address_at_diagnosis_county == "CA") %>% 
  filter(address_at_diagnosis_city != "Logan") %>% 
  filter(place_name == "") %>% 
  dplyr::select(patient_record_number, person_last_name, person_first_name, first_positive_lab, patient_birth_date, address_at_diagnosis_county, address_at_diagnosis_city, first_positive_lab)


#5-17 Logan- Not investigated, missing school name
minors_ni_lo <- epitraxudoh_merge %>% 
  filter(covid_case_date >= "2021-08-18") %>% 
  filter(patient_age_at_event_in_years <= 17, patient_age_at_event_in_years >= 5) %>% 
  filter(address_at_diagnosis_county == "CA") %>% 
  filter(address_at_diagnosis_city == "Logan") %>%
  filter(place_name == "") %>% 
  dplyr::select(patient_record_number, person_last_name, person_first_name, first_positive_lab, patient_birth_date, address_at_diagnosis_county, address_at_diagnosis_city)


#5-17 Rich- Not investigated, missing school name
minors_ni_ri <- epitraxudoh_merge %>% 
  filter(covid_case_date >= "2021-08-30") %>% 
  filter(patient_age_at_event_in_years <= 17, patient_age_at_event_in_years >= 5) %>%
  filter(address_at_diagnosis_county == "RI") %>% 
  filter(place_name == "") %>% 
  dplyr::select(patient_record_number, person_last_name, person_first_name, first_positive_lab, patient_birth_date, address_at_diagnosis_county, address_at_diagnosis_city)


write.csv(minors_ni_be, "C:/Users/tstoker/Desktop/RWD/minors_ni_be.csv")
write.csv(minors_ni_ca, "C:/Users/tstoker/Desktop/RWD/minors_ni_ca.csv")
write.csv(minors_ni_lo, "C:/Users/tstoker/Desktop/RWD/minors_ni_lo.csv")
write.csv(minors_ni_ri, "C:/Users/tstoker/Desktop/RWD/minors_ni_ri.csv")

###########
#New minors not investigated
last_minors_ni_be <- read.csv("last_minors_ni_be.csv", header = T)
last_minors_ni_ca <- read.csv("last_minors_ni_ca.csv", header = T)
last_minors_ni_lo <- read.csv("last_minors_ni_lo.csv", header = T)
last_minors_ni_ri <- read.csv("last_minors_ni_ri.csv", header = T)

colnames(last_minors_ni_be)
colnames(minors_ni_be)

#Merge old and new k12 minor cases
minors_be_merged <- rbind(last_minors_ni_be, minors_ni_be, all = TRUE)
minors_ca_merged <- rbind(last_minors_ni_ca, minors_ni_ca, all = TRUE)
minors_lo_merged <- rbind(last_minors_ni_lo, minors_ni_lo, all = TRUE)
minors_ri_merged <- rbind(last_minors_ni_ri, minors_ni_ri, all = TRUE)

#Remove duplicates, leaving only new minors k12 cases that need school info
minors_be_new <- minors_be_merged[!(duplicated(minors_be_merged$patient_record_number) | duplicated(minors_be_merged$patient_record_number, fromLast = TRUE)), ] 
minors_ca_new <- minors_ca_merged[!(duplicated(minors_ca_merged$patient_record_number) | duplicated(minors_ca_merged$patient_record_number, fromLast = TRUE)), ] 
minors_lo_new <- minors_lo_merged[!(duplicated(minors_lo_merged$patient_record_number) | duplicated(minors_lo_merged$patient_record_number, fromLast = TRUE)), ] 
minors_ri_new <- minors_ri_merged[!(duplicated(minors_ri_merged$patient_record_number) | duplicated(minors_ri_merged$patient_record_number, fromLast = TRUE)), ] 

##ONLY WRITE IF THE ABOVE STEP WORKED###
write.csv(minors_ni_be, "last_minors_ni_be.csv")
write.csv(minors_ni_ca, "last_minors_ni_ca.csv")
write.csv(minors_ni_lo, "last_minors_ni_lo.csv")
write.csv(minors_ni_ri, "last_minors_ni_ri.csv")

write.csv(minors_be_new, "minors_be_new.csv")
write.csv(minors_ca_new, "minors_ca_new.csv")
write.csv(minors_lo_new, "minors_lo_new.csv")
write.csv(minors_ri_new, "minors_ri_new.csv")

###################################################
#Adult school cases at UDOH that need routed to us
adult_school_cases <- epitraxudoh_merge %>% 
  filter(patient_jurisdiction_of_investigation == "Utah State") %>% 
  filter(covid_interview_complete == "f") %>% 
  filter(corona_pui_col_school_univ == "Yes") %>%  
  filter(patient_birth_date <= "2003-11-02") %>% 
  filter(covid_case_date >= "2021-11-05") %>% 
  filter(corona_pui_col_school_univ_type != "Volunteer/visitor") %>%
  filter(corona_pui_col_school_univ_type != "volunteer-visitor") %>%
  filter(exposure_place_school_district %in% c("Bear River Charter School","Box Elder District","Bridgerland Technical College",
                                               "Cache District","The Center for Creativity Innovation and Discovery","Edith Bowen Laboratory School","Fast Forward High","Intech Collegiate Academy",
                                               "Logan City District","Promontory School of Expeditionary Learning","Rich District",
                                               "Thomas Edison","Thomas Edison - South - Thomas Edison", "")) %>% 
  distinct(patient_record_number, .keep_all = TRUE) %>% 
  dplyr::select(patient_record_number, patient_birth_date, covid_case_date, patient_event_onset_date, person_first_name, person_last_name)

write.csv(adult_school_cases, "C:/Users/tstoker/Desktop/RWD/adult_school_cases.csv")

################
#Scratch Code for Surveillance

#Check individual school
LV <- k12 %>% 
  filter(place_name == "Lake View Elementary School - Box Elder District") %>% 
  filter(covid_case_date >= todays_date - 13) %>% 
  dplyr::select(patient_record_number, patient_birth_date, covid_case_date, patient_event_onset_date, person_first_name, person_last_name)

write.csv(LV, "C:/Users/tstoker/Desktop/RWD/LV.csv")

green <- k12 %>% 
  filter(place_name == "Green Canyon High School - Cache District") %>% 
  filter(covid_case_date >= todays_date - 7) %>% 
  dplyr::select(patient_record_number, covid_case_date, patient_event_onset_date, person_first_name, person_last_name)

garland <- k12 %>% 
  filter(place_name == "Garland Elementary School - Box Elder District") %>% 
  filter(covid_case_date >= todays_date - 14) %>% 
  dplyr::select(patient_record_number, covid_case_date, patient_event_onset_date, person_first_name, person_last_name)

ACYI <- k12 %>% 
  filter(place_name == "Alice C. Harris Intermediate - Box Elder District") %>% 
  filter(covid_case_date >= todays_date - 14)

ccid <- k12 %>% 
  filter(place_name == "The Center for Creativity Innovation and Discovery") %>% 
  filter(covid_case_date >= todays_date - 14) 

richsele <- k12 %>% 
  filter(place_name == "South Rich Elementary - Rich District") %>% 
  filter(covid_case_date >= todays_date - 14)  

richmid <- k12 %>% 
  filter(place_name == "Rich Middle School - Rich District") %>% 
  filter(covid_case_date >= todays_date - 14)  



richhigh <- k12 %>% 
  filter(place_name == "Rich High School - Rich District") %>% 
  filter(covid_case_date >= todays_date - 14)  

ebl <- k12 %>% 
  filter(place_name == "Edith Bowen Laboratory School") %>% 
  filter(covid_case_date >= todays_date - 13)

tomes <- k12 %>% 
  filter(place_name %in% c("Thomas Edison - South - Thomas Edison", "Thomas Edison Charter Schools South")) %>% 
  filter(covid_case_date >= todays_date - 13)

tomesall <- k12 %>% 
  filter(place_name %in% c("Thomas Edison - South - Thomas Edison", "Thomas Edison Charter Schools South"))

#Check today's totals, incidence rate and attack rate
today <- k12_grouped %>% 
  filter(Date == "2021-09-30")
###########################################################################


