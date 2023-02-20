########################## ############################ ##############################
########################## COV NAME LIST (used from R5) ##############################
########################## ############################ ##############################


#' from R5 all categorical variables should be character/factors with named levels
#' This list is for subbing in informative category names to tables 

cov_name_list <- list(sex = "Sex",
                      age_group_named="Age group", 
                      age="Age", 
                      shielding = "Shielding out of concern over COVID-19",
                      clin_vulnerable = "Advised to shield by medical professional/ NHS", 
                      carehome = "Care home resident",
                      hh_size_cat ="Household size",
                      pregnant = "Pregnant",
                      gross_household_cat = "Gross household income",
                      imd_quintile_cat = "Index of multiple deprivation (IMD) quintile",
                      edu_cat = "Level of education",
                      num_child_cat = "Number of children in house",
                      child_in_house = "Child in house",
                      covida_cat= "Previous case of COVID" ,
                      covidc_cat = "Severity of COVID-19 symptoms", 
                      covid_severity = "Treatment sought/ received for COVID-19", 
                      symptomatic = "Symptomatic",
                      region_named = "Region" , 
                      ethnic_new = "Ethnicity", 
                      smokenow = "Current smoker", 
                      smokeever = "Ever smoker",
                      vapenow = "Current vaper",
                      smokeyears= "Years of smoking", 
                      smokeyears_cat = "Years of smoking",
                      smoke_intensity = "Smoking intensity", 
                      smoking_status_5cat = "Smoking status/ quit date",
                      years_since_quitting_cat = "Years since quitting smoking",
                      age_group_jvci = "Age group (JCVI categories)",
                      bmi_cat = "Adiposity",
                      age_group_paul = "Age",
                      worker_status = "Keyworker/ work status",
                      ethnic19_char = "Ethnicity (granular)",
                      work1_healthcare_worker = "Healthcare worker", 
                      work1_carehome_worker = "Care home worker", 
                      work1_other_key_worker = "Other key worker",
                      work1_other_not_key_worker = "Other worker (not key worker)",
                      work1_other_dont_know = "Work: don't know",
                      work2_1_homedelivery = "Home delivery worker",
                      work2_2_retail = "Retail worker",
                      work2_3_police_prison = "Police, prisons, fire & rescue",
                      work2_4_pub_trans = "Public transport worker",
                      work2_5_education = "Education, school or nursey worker",
                      work2_6_armed_forces = "Armed forces",
                      work2_8_not_public_facing = "Other public facing role",
                      work2_9_homeworker = "Not currently required to work outside the home",
                      work2_10_hospitality = "Hospitality worker",
                      work2_11_personal_care = "Personal care, eg hairdresser, beauty therapist, personal trainer",
                      work2_12_childcare = "Childcare worker",
                      workstudypers1reg_cat = "Education worker level",
                      work1_healthcare_or_carehome_worker <- "Healthcare or care home worker",
                      hospitalised_covid <- "Hospitalised with COVID",
                      dummy="All participants",
                      all_participants="All participants",
                      res="LFIA test result",
                      mask_indoors = "Mask wearing indoors",
                      mask_outdoors = "Mask wearing outdoors",
                      face_covering = "Face covering",
                      imd_quintile = "Index of Multiple Deprivation (IMD)",
                      imd_quintile_cat = "Index of Multiple Deprivation (IMD)"
)



########## MAIN CATS / COVS list DEPRECATED FROM R5 ################


### create a combined list of lists for covs, cats, and ref levels
cat_cov_list <- list(gender = list("Sex",c("Male", "Female"),1),
                     age_group = list("Age", c("18-24", "25-34", "35-44","45-54", "55-64","65-74", "75+"),3),
                     ethnic_num = list("Ethnicity",c("White","Mixed","Asian",
                                                     "Black",
                                                     "Other"),1),
                     region = list("Region",c("North East", "North West", "Yorkshire and The Humber",
                                              "East Midlands","West Midlands","East of England", 
                                              "London", "South East", "South West"),8),
                     imd_quintile = list("IMD quintile",c("Most deprived: 1","2","3","4","Least deprived: 5"),5),
                     edu_num = list("Highest educational level reached", 
                                    c("No qualification","Other",
                                      "GCSE","Post-GCSE qualification","Degree or above"),1),
                     gross_household_cat = list("Gross household income", 
                                                c("£0-14,999","£15,000-49,999","£50,000-149,999",
                                                  ">£150,000"),2),
                     work_new = list("Employment",c("Healthcare (patient-facing)",
                                                    "Healthcare (other)",
                                                    "Care home (client-facing)",
                                                    "Care home (other)",
                                                    "Other essential worker",
                                                    "Other worker",
                                                    "Not in employment"), 6),
                     employment = list("Employment (detailed)", c("Full time", "Part time", "Self-employed", 
                                                                  "Govt supported training", "Unemployed", "Retired",
                                                                  "Student", "Looking after home/family", "Sick/disabled",
                                                                  "Other", "Prefer not to say"), 1),
                     work_sector = list("Employment (sector)", c("Home delivery",
                                                                 "Retail",
                                                                 "Emergency services, prisons, coastguard",
                                                                 "Public transport or taxi", "Teacher or childcare",
                                                                 "Armed forces", "Other", "Not working outside home"), 8),
                     hh_size_cat = list("Household size",c("1", "2", "3", "4", "5", "6", "7+"),1),
                     child_in_house = list("One or more children (under 18) in household", c("No","Yes"), 1),
                     
                     COVIDC = list("History of COVID-19 symptoms",
                                   c("No symptoms", "Mild symptoms", "Moderate symptoms", "Severe symtoms"),1),
                     bmi_cat = list("BMI",c("Underweight (<18.5)","Normal (18.5-24.9)", 
                                            "Overweight (25-29.9)", "Obese (>=30)"),2),
                     smokenow = list("Current smoker",c("Yes", "No"),2),
                     popDens_quintile = list("Population density quintile", c("1","2","3","4","5"),1),
                     COVIDA = list("History of COVID-19", c("Positive test", 
                                                            "Suspected by doctor", 
                                                            "Suspected by respondent", 
                                                            "No"),4),
                     PCRpos_cat = list("Time since positive PCR test", 
                                       c("<30 days ago","30-60 days ago",
                                         "61-90 days ago","90+ days ago"),1),
                     SympOnset_cat = list("Time since COVID-19 symptom onset", 
                                          c("<30 days ago","30-60 days ago",
                                            "61-90 days ago","91-120 days ago",
                                            "121-150 days ago", ">150 days ago"),1),
                     COVIDCON = list("Contact with COVID-19 case",
                                     c("Yes, with confirmed case", "Yes, with suspected case","No"),3),
                     
                     health_conditions = list("Number of existing health conditions",
                                              c(">1", "1","0"),3),
                     
                     
                     bmi_lower = list("BMI (lower)",c("Underweight (<18.5)","Normal (18.5-23.4)", 
                                                      "Overweight (23.5-27.4)", "Obese (>=27.5)"),2),
                     CAREHOME = list("Care home resident",c("Yes", "No"), 2),
                     symptoms_cat = list("Reported symptoms of previous COVID-19 case", 
                                         c("No symptoms", "Atypical symptoms only", "Screening symptoms"),1),
                     num_child = list("Number of children (under 18) in household", c("0","1", "2", ">2"),1),
                     
                     covid_severity = list("COVID outcome severity", c("No treatment", "Sought medical care",
                                                                       "Hospital admission", "ICU"), 1),
                     num_child_under_5 = list("Number of children under 5 in household", c("0","1", "2", ">2"),1),
                     num_child_5_to_10 = list("Number of children aged 5 to 10 in household", c("0","1", "2", ">2"),1),
                     num_child_11_to_17 = list("Number of children aged 11 to 17 in household", c("0","1", "2", ">2"),1),
                     ethnic = list("Ethnicity (granular)", c("White British", "White Irish", "White gypsy or Irish traveller",
                                                             "Any other white background", 
                                                             "Mixed white / black Carribean", "mixed white / black African",
                                                             "Mixed white and Asian", "Any other mixed / multiple ethnic background",
                                                             "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian",
                                                             "African", "Caribbean", "Other black / African / Caribbean background",
                                                             "Arab", "Any other ethnic group", "Prefer not to say"), 1),
                     work_type_broad = list("Employment (broad)", c("Healthcare", "Care home", 
                                                                    "Other essential worker",
                                                                    "Other worker","Not in employment"), 4),
                     age_group_broad = list("Age group (broad)", c("18-24", "25-44", "45-64", "65+"), 2),
                     age_group_very_broad = list("Age group (very broad)", c("18-34", "35-64", "65+"), 1),
                     household_type = list("Household type (65+ respondents", c("Single", "Two older adults", 
                                                                                "Multi-generational (no children)", 
                                                                                "Multi-generational (with children",
                                                                                "Three or more older adults"), 2),
                     multigenerational_household = list("Older adult in multigenerational household (y/n)", c("No", "Yes"), 1),
                     
                     ### Adding any variables that are only present in later rounds at the end here so we can easily subset
                     pregnant = list("Pregnant at time of test", c("Yes", "No", "Prefer not to say"), 2),
                     # smoke_years_current_cat = list("Years of smoking", 
                     #                                c("<10","11-20","21-30","31-40","41-50","50+"),1),
                     face_covering = list("Face covering", c("No face covering", "Face covering at work/school only", 
                                                             "Face covering in other situations only", "Face covered in most situations",
                                                             "Face covered anyway for other reasons"), 1),
                     furlough = list("Furlough or redundancy", c("Furloughed", "Made redundant", "Not furloughed or redundant"), 3),
                     campus = list("Student living arrangements", c("Student halls", "Private accomodation with other students",
                                                                    "Private accomodation (no other students)", 
                                                                    "Parental home","Other"), 4),
                     overseas_travel = list("Travel overseas in past 3 months", c("Yes", "No"), 2),
                     # smoke_years_current_cat = list("Number of years of smoking", c("Less than 10 years","11-20 years", "21-30 years",
                     #                                                                "31-40 years","41-50 years","More than 50 years"), 1)
                     
                     # New variables for round 5
                     
                     
                     vaccinated = list("Ever had a coronavirus vaccine", c("Yes", "No", "As part of trial (could be placebo)"),2),
                     vaccine_doses = list("How many vaccine doses so far?", c("One", "Two", "More than two"), 1),
                     vaccine_type = list("Which vaccine did you receive?", c("Pfizer/BioNTtech","AstraZeneca/Oxford","Moderna","don't know"), 1)
)



### add names to cat_cov_list
for(i in 1:length(cat_cov_list)){
  names(cat_cov_list[[i]]) <- c("Description", "Levels", "Reference_level")
}






# Covariates for stability analysis prediction ----------------------------

stab_covs <- list(sociodemographic = c("age", "gender", "ethnic", "NADULTS", "NCHILD", "employment", "edu_num","imd_score",
                                       "gross_household_cat","popd18_km2"),
                  health = c("bmi", "health_conditions", "smokenow"),
                  behavioural = c("behav_pre_hygiene", "behav_pre_social"))



stab_covs_comorbid <- list(sociodemographic = c("age", "gender", "ethnic", "NADULTS", "NCHILD", "employment", "edu_num","imd_score",
                                                "popd18_km2"),
                           health = c("bmi", "smokenow", paste0("HEALTHA_0",c(1:4,6:9)) , paste0("HEALTHA_",10:17)))


### list of all health conditions
health_conditions <- c("Organ transplant recipient","Diabetes (type I or II)",
                       "Heart disease or heart problems",
                       "Hypertension",
                       # "Overweight", # removing this as so much missing data
                       "Stroke","Kidney disease",
                       "Liver disease","Anemia","Asthma",
                       "Other lung condition","Cancer",
                       "Neurological condition",
                       "Immunocompromised*",
                       "Depression","Anxiety","Psychiatric disorder (other)",
                       "None of these")

### list of all symptoms (in order of appearance in data)
sympnames <- c("Appetite loss", "Nausea/vomiting", "Diarrhoea", "Abdominal pain / belly ache",
               "Runny nose", "Sneezing", "Blocked nose", "Sore eyes", "Loss or change of sense of smell",
               "Loss or change of sense of taste", "Sore throat", "Hoarse voice", "Headache", "Dizziness",
               "Shortness of breath", "New persistent cough", "Tight chest", "Chest pain", "Fever",
               "Chills", "Difficulty sleeping", "Tiredness", "Severe fatigue", "Numbness/tingling",
               "Heavy arms/legs", "Muscle aches", "No symptoms", "Red, itchy areas on skin", 
               "Sudden swelling to face or lips",
               "Purple sores/blisters on feet")


### list of all symptoms (in order of appearance in data)
long_sympnames <- c("Appetite loss", "Nausea/vomiting", "Diarrhoea", "Abdominal pain / belly ache",
               "Runny nose", "Sneezing", "Blocked nose", "Sore eyes", "Loss or change of sense of smell",
               "Loss or change of sense of taste", "Sore throat", "Hoarse voice", "Headache", "Dizziness",
               "Shortness of breath", "New persistent cough", "Tight chest", "Chest pain", "Fever",
               "Chills", "Difficulty sleeping", "Tiredness", "Severe fatigue", "Numbness/tingling",
               "Heavy arms/legs", "Muscle aches", "No symptoms", "Red, itchy areas on skin", 
               "Sudden swelling to face or lips",
               "Purple sores/blisters on feet")



# add symptom names to cov_name_list
sympnames_list <- as.list(sympnames)
names(sympnames_list) <- c(paste0("covidsym_0", 1:9),paste0("covidsym_", 10:30))
cov_name_list <- c(cov_name_list,sympnames_list)



### list of all symptoms (in order of appearance in REACT-1 data)
react1_sympnames <- c("Loss or change of sense of smell", "Loss or change of sense of taste", "New persistent cough", 
  "Fever", "Runny nose", "Sneezing", "Blocked nose", "Sore eyes", 
  "Sore throat", "Hoarse voice", "Headache", "Dizziness", "Appetite loss", 
  "Nausea/vomiting", "Diarrhoea", "Abdominal pain / belly ache", 
  "Shortness of breath", "Tight chest", "Chest pain", 
  "Chills", "Difficulty sleeping", 
  "Tiredness", "Severe fatigue", 
  "Numbness/tingling", "Heavy arms/legs",
  "Muscle aches", "No symptoms", "Don't know")



# React-1 long covid symptom names ----------------------------------------



react1_longcovidsymptoms <- c("Loss or change to sense of smell", "Loss or change to sense of taste", "Fever", "Headaches", 
  "Confusion, 'brain fog', forgetfulness", "Dizziness, vertigo", 
  "Abdominal issues (stomach ache, diarrhoea, nausea)", "Shortness of breath", 
  "Tightness in chest, chest pain", "Heart issues (racing heart, palpitations, irregular heartbeat etc)", 
  "Coughing", "Sneezing", "Runny nose", "Mild fatigue (e.g. feeling tired)", 
  "Severe fatigue (e.g. inability to get out of bed)", "Numbness or tingling somewhere in the body", 
  "Achy or cramping muscles, pain in muscles", "Pain in joints", "Difficulty sleeping", "Loss of appetite", 
  "Itchy, sore or red eyes", "Vision issues", "Hearing issues (e.g. hearing loss, Tinnitus etc)", 
  "Hair loss", "Sore throat or hoarse voice", "Skin issues (itchy, scaly, redness, etc)", 
  "Sudden swelling of the face or lips", "Red/purple sores or blisters on your feet (including toes)", "Something else")

# Imperial colour palette -------------------------------------------------




### Imperial colour palette
myCols <- c("#002147", # Navy
            "#006EAF", # Blue
            "#00ACD7", # Pool blue
            "#379f9f", # Seaglass
            "#9D9D9D", # Cool grey
            "#BBCE00", # Lime
            "#D24000", # Orange
            "#E40043", # Cherry
            "#960078", # Violet
            "#FFDD00" # Lemon yellow
)












########## DEPRECATED CATS / COVS list ################
### (retained to avoid breaking soe older code) #######



#' Specify the covariates of interest and the levels of each covariate.
covs <- c("gender", "age_group", "region", "work_new", "ethnic_num",
          "hh_size_cat", "imd_quintile","popDens_quintile","COVIDA", "COVIDC", "PCRpos_cat", 
          "SympOnset_cat", paste0("COVIDSYM_0",1:9), paste0("COVIDSYM_",10:27),
          "COVIDCON", "heath_conditions",
          "bmi_cat","smokenow",  "CAREHOME")

ref.levels <- c(1,3,8,6,1,1,5,1,4,1,1,1,rep(1,27),3,3,2,2,2)


cats <- list(
  gender = c("Male", "Female"),
  age_group = c("18-24", "25-34", "35-44","45-54", "55-64","65-74", "75+"),
  region = c("North East", "North West", "Yorkshire and The Humber",
             "East Midlands","West Midlands","East of England", 
             "London", "South East", "South West"),
  work_type = c("Healthcare (patient-facing)",
                "Healthcare (other)",
                "Care home (client-facing)",
                "Care home (other)",
                "Other essential worker",
                "Other worker",
                "Not in employment"),
  ethnic_num = c("White","Mixed","Asian",
                "Black",
                 "Other"),
  hh_size = c("1", "2", "3", "4", "5", "6", "7+"),
  imd = c("Most deprived: 1","2","3","4","Least deprived: 5"),
  popDens_quintile =  c("1","2","3","4","5"),
  COVIDA = c("Positive test", "Suspected by doctor", "Suspected by respondent", "No"),
  COVIDC = c("No symptoms", "Mild symptoms", "Moderate symptoms", "Severe symtoms"),
  PCRpos_cat = c("<30 days ago","30-60 days ago","61-90 days ago","90+ days ago"),
  SympOnset_cat = c("<30 days ago","30-60 days ago","61-90 days ago","91-120 days ago",
                    "121-150 days ago", ">150 days ago"),
  COVIDSYM_01 = c("No", "Yes"),
  COVIDSYM_02 = c("No", "Yes"),
  COVIDSYM_03 = c("No", "Yes"),
  COVIDSYM_04 = c("No", "Yes"),
  COVIDSYM_05 = c("No", "Yes"),
  COVIDSYM_06 = c("No", "Yes"), 
  COVIDSYM_07 = c("No", "Yes"),
  COVIDSYM_08 = c("No", "Yes"),
  COVIDSYM_09 = c("No", "Yes"),
  COVIDSYM_10 = c("No", "Yes"),
  COVIDSYM_11 = c("No", "Yes"),
  COVIDSYM_12 = c("No", "Yes"),
  COVIDSYM_13 = c("No", "Yes"),
  COVIDSYM_14 = c("No", "Yes"),
  COVIDSYM_15 = c("No", "Yes"),
  COVIDSYM_16 = c("No", "Yes"),
  COVIDSYM_17 = c("No", "Yes"),
  COVIDSYM_18 = c("No", "Yes"),
  COVIDSYM_19 = c("No", "Yes"),
  COVIDSYM_20 = c("No", "Yes"),
  COVIDSYM_21 = c("No", "Yes"),
  COVIDSYM_22 = c("No", "Yes"),
  COVIDSYM_23 = c("No", "Yes"),
  COVIDSYM_24 = c("No", "Yes"),
  COVIDSYM_25 = c("No", "Yes"),
  COVIDSYM_26 = c("No", "Yes"),
  COVIDSYM_27 = c("No", "Yes"),
  COVIDCON= c("Yes, with confirmed case", "Yes, with suspected case","No"),
  health_conditions = c(">1", "1","0"),
  bmi_cat =c ("Underweight (<18.5)","Normal (18.5-24.9)", 
              "Overweight (25-29.9)", "Obese (>=30)"),
  smoke = c("Yes", "No"),
  CAREHOME = c("Yes", "No")
) 



# Vaccine reluctance vars -------------------------------------------------

vacc_refuse_reasons = c("Worried about side effects",
                        "Want to wait and see how well vaccine works",
                        "Worried about long-term health effects",
                        "Worried about risk of travel to vaccination centre",
                        "Too difficult to get to vaccination centre",
                        "Do not feel COVID-19 is a personal risk",
                        "Worried about effect on existing health condition",
                        "Against vaccines in general",
                        "Do not think it will work for me",
                        "Worried the vaccine will give me COVID-19",
                        "Worried it might be painful",
                        "Worried it will make me feel ill",
                        "Don't need it as already had COVID-19",
                        "Am pregnant/breastfeeding and worried about effects on baby",
                        "Impact of COVID-19 is greatly exaggerated",
                        "Don't trust the people who developed the vaccine",
                        "So long as other people get the vaccine it doesn't matter if I don't",
                        "Doses are limited and other people need it more","Other","Prefer not to say")


vacc_refuse_reasons_short <- c("Side effects",
                               "Wait see if vaccine works",
                               "Long term health effects",
                               "Travel to vacc centre",
                               "Can't get to vacc centre",
                               "COVID not a risk",
                               "Existing health conditions",
                               "Against vaccines",
                               "Won't work for me",
                               "Will give me COVID",
                               "Might be painful",
                               "Make me ill",
                               "Alredy had COVID",
                               "Pregnant/breastfeeding",
                               "COVID is exaggerated",
                               "Don't trust vax developers",
                               "Don't need if others get it",
                               "Others are more in need",
                               "Other",
                               "Prefer not to say"
                               )





# GGplot theme adjustment -------------------------------------------------
library(ggplot2)
theme_adjust <- theme(strip.background = element_rect(fill = "white"),
                      strip.text = element_text(face = "bold"))


