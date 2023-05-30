#This script merges long-term employment projects from https://projectionscentral.org/Projections/rest_and_downloads, and
#education requirement data from the BLS, table 5.4 https://www.bls.gov/emp/tables/education-and-training-by-occupation.htm

#### load BLS SOC crosswalk ####
#### https://www.bls.gov/soc/2018/soc_2010_to_2018_crosswalk.xlsx
soc_2010_2018_xwalk <- readxl::read_excel(here('input/soc_2010_to_2018_crosswalk.xlsx'), skip = 8) %>% 
  janitor::clean_names() %>% 
  rename(code2010 = x2010_soc_code,
         code2018 = x2018_soc_code,
         title2010 = x2010_soc_title,
         title2018 = x2018_soc_title)

##### Load 2030 projections. ####
projections_data <- read_csv(here('input/ltprojections.csv')) %>% 
  rename(area_name = areaname,
         proj_occ_name = name,
         project2030=proj) %>% 
  #create occ_code variable by stripping hyphens from code
  mutate(occupation_code = as.numeric(str_remove(code, '-'))) %>% 
  select(stfips, area_name, code, proj_occ_name, project2030) %>% 
  mutate(
    # Public Relations and Fundraising Managers (SOC 2018)
    code = str_replace(code, '11-2030', '11-2031'),
    # Administrative Services and Facilities Managers (SOC 2018)
    code = str_replace(code, '11-3010', '11-3011'), 
    # Managers, all other
    code = str_replace(code, '11-9198', '11-9199'),
    # Business operations specialists, all other
    code = str_replace(code, '13-1198', '13-1199'),
    #Property Appraisers and Assessors
    code = str_replace(code, '13-2020', '13-2021'),
    # Financial specialists, all other
    code = str_replace(code, '13-2098', '13-2099'), 
    # Database Administrators and Architects
    code = str_replace(code, '15-1245', '15-1141'),
    # Software Developers and Software Quality Assurance Analysts and Testers
    code = str_replace(code, '15-1256', '15-1132'),
    # Web Developers and Digital Interface Designers
    code = str_replace(code, '15-1257', '15-1134'),
    # Data Scientists and Mathematical Science Occupations, All Other
    code = str_replace(code, '15-2098', '15-2099'),
    # Calibration and Engineering Technologists and Technicians, Except Drafters, All Other
    code = str_replace(code, '17-3098', '17-3029'),
    # Agricultural and Food Science Technicians (SOC 2018)
    code = str_replace(code, '19-4010', '19-4011'),
    # Geological and Hydrologic Technicians
    code = str_replace(code, '19-4045', '19-4041'),
    # Teachers and Instructors, All Other, Except Substitute Teachers
    code = str_replace(code, '25-3097', '25-3099'),
    # Miscellaneous Entertainers and Performers, Sports and Related Workers
    code = str_replace(code, '27-2090', '27-2099'),
    # Lighting Technicians and Media and Communication Equipment Workers, All Other
    code = str_replace(code, '27-4098', '27-4099'),
    # Physicians, All Other; and Ophthalmologists, Except Pediatric
    code = str_replace(code, '29-1228', '29-1069'),
    # Surgeons, Except Ophthalmologists
    code = str_replace(code, '29-1248', '29-1067'),
    # Acupuncturists and Healthcare Diagnosing or Treating Practitioners, All Other
    code = str_replace(code, '29-1298', '29-1199'),
    # Emergency Medical Technicians and Paramedics (SOC 2018)
    code = str_replace(code, '29-2040', '29-2041'),
    # Medical Dosimetrists, Medical Records Specialists, and Health Technologists and Technicians, All Other
    code = str_replace(code, '29-2098', '29-2099'),
    # Health Information Technologists, Medical Registrars, Surgical Assistants, & Healthcare Practitioners, AO
    code = str_replace(code, '29-9098', '29-9099'),
    # Miscellaneous First-Line Supervisors, Protective Service Workers
    code = str_replace(code, '33-1090', '33-1099'),
    # School Bus Monitors and Protective Service Workers, All Other
    code = str_replace(code, '33-9098', '33-9099'),
    # 39-1098 First-Line Supervisors of Personal Service and Entertainment and Recreation Workers, Except Gambling Services
    code = str_replace(code, '39-1098', '39-1021'),
    # Crematory Operators and Personal Care and Service Workers, All Other
    code = str_replace(code, '39-9098', '39-9099'),
    # Aircraft Service Attendants and Transportation Workers, All Other
    code = str_replace(code, '53-6098', '53-6099')
  ) %>%
  left_join(soc_2010_2018_xwalk, by=c('code'='code2010'))
    

#### Load BLS education matrix, and merge crosswalk #### 
#Education matrix uses 2018 SOC codes
educ_matrix <- readxl::read_excel(here('input/bls_education_matrix_5.4.xlsx'), range = "A2:E834") %>% 
  janitor::clean_names() %>% 
  rename(bls_name = x2021_national_employment_matrix_title,
         code = x2021_national_employment_matrix_code,
         educ_req = typical_education_needed_for_entry,
         experience_req = work_experience_in_a_related_occupation,
         training_req = typical_on_the_job_training_needed_to_attain_competency_in_the_occupation) %>% 
  
  #match SOC code for Property Appraisers and Assessors, from 13-2020 to 13-2021
  mutate(code = str_replace(code, '13-2020', '13-2021')) %>% 
  #Create 4-category education variable
  mutate(educ2 = case_when( educ_req %in% c('No formal educational credential') ~ 1,
                            educ_req %in% c('High school diploma or equivalent') ~ 2,
                            educ_req %in% c('Associate\'s degree','Postsecondary nondegree award','Some college, no degree') ~ 3,
                            educ_req %in% c('Bachelor\'s degree','Doctoral or professional degree','Master\'s degree') ~ 4)) %>% 
  #Add labels to factor
  set_value_labels(educ2 = c('Less than high school' = 1, 'High school'= 2, 'Some college' = 3, 'BA or greater'= 4)) %>% 
  #Mutate strings to upper
  select(-training_req, -experience_req)

