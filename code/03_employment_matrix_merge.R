#This script analyzes the cleaned excel files from 02_clean_data

#Merge long-term job projection data and BLS education requirements matrix
projections_education_merge <- projections_data %>% 
  filter(code!='00-0000') %>% 
  left_join(educ_matrix, by='code')

#Create dataframe of all successful merges
merge_1 <- projections_education_merge %>% 
  filter(!is.na(educ2))

#Perform second merge of projections/education data on 2018 SOC codes
merge_2 <- projections_education_merge %>%
  filter(is.na(educ_req)) %>% 
  #remove empty columns
  select(-bls_name, -educ_req, -educ2) %>% 
  #merge education matrix a second time, on 2018 SOC codes
  left_join(educ_matrix, by=c('code2018'='code')) %>% 
  filter(!is.na(educ2))

# Create custom crosswalk for projection codes mapped onto multiple BLS SOC codes
custom_xwalk <- soc_2010_2018_xwalk %>% 
  filter(code2010 %in% c(
    #Underground Mining Machine Operators and Extraction Workers, All Other
    '47-5042', '47-5049', '47-5099',
    #Earth Drillers, Except Oil and Gas; and Explosives Workers, Ordnance Handling Experts...
    '47-5021', '47-5031',
    #Passenger Vehicle Drivers, Except Bus Drivers, Transit and Intercity
    '53-3022','53-3041')) %>% 
  mutate(newcodes = ifelse(code2010 %in% c('47-5042', '47-5049', '47-5099'), yes = '47-5098', 
                           ifelse(code2010 %in% c('47-5021', '47-5031'), yes = '47-5097',
                                  ifelse(code2010 %in% c('53-3022','53-3041'), yes= '53-3058', no=NA))))

#Isolate occupations that failed to merge to education matrix
failed_merges <- projections_education_merge %>% 
  filter(is.na(educ2) & is.na(title2010)) %>% 
  select(stfips, area_name, code, proj_occ_name, title2018, project2030) %>% 
  #Merge with custom crosswalk
  left_join(custom_xwalk, by=c('code'='newcodes'))

#Perform third merge of projections data to BLS education matrix
merge_3 <- failed_merges %>% 
  left_join(educ_matrix, by=c('code2018'='code')) %>% 
  select(stfips, area_name, code, proj_occ_name, project2030, educ2)

#Append three projections-education requirement merges
appended_merges <- bind_rows(merge_1, merge_2, merge_3) %>% 
  select(stfips, area_name, code, proj_occ_name, project2030, title2018, educ2) %>% 
    # Divide projections by sum of codes to avoid double-counting of employment projections
    mutate(code_n = n(),
           adj_projection = project2030/code_n, .by=c('stfips', 'code')) 

#Produce table of jobs and required educational attainment by state
state_projections_needs <- appended_merges %>% 
  mutate(educ2 = to_factor(educ2)) %>% 
  summarise(total_proj = sum(adj_projection, na.rm=TRUE), .by = c(area_name, educ2)) %>% 
  pivot_wider(id_cols = c(area_name), names_from = educ2, values_from = total_proj) %>% 
  arrange(area_name) %>%
  mutate(Total = `BA or greater`+`High school` + `Less than high school` + `Some college`) %>% 
  #filter out Guam, PR, and Virgin Islands
  filter(!(area_name %in% c('Guam', 'Puerto Rico', 'Virgin Islands'))) %>% 
  write_csv(here('output/EduProjections2030.csv'))


