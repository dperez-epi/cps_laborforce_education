#script to load and clean CPS basic data

#Load geographic labels
geographic_labels <- read_csv(here('input/geographic_labels.csv'))


#Load CPS Basic using epiextractr
cps <- load_basic(1980:2022, year, month, basicwgt, educ, statefips, lfstat, age) %>% 
  filter(year %in% c(1980, 1990, 2000, 2010, 2012, 2021, 2022)) %>% 
  filter(age>=16) %>% 
  #Create indicator for being in/out of labor force
  mutate(labforce = ifelse(lfstat %in% c(1,2),  yes=1, no=0)) %>% 
  #New education variable
  mutate(educ2 = case_when(educ==1 ~ 1,
                           educ==2 ~ 2,
                           educ==3 ~ 3,
                           educ %in% c(4,5) ~ 4)) %>% 
  set_value_labels(labforce = c('In labor force' = 1, 'Not in labor force'=0),
                   educ2 = c('Less than high school'=1, 'High school'=2, 'Some college'=3, 'BA or greater'= 4)) %>% 
  left_join(geographic_labels)



################ US tabulations ################ 

#Data frame tabulating size of US Labor force
us_labforce <- cps %>% 
  filter(labforce==1) %>% 
  group_by(year) %>% 
  #sum total labor force
  summarize(total_labor_force = sum(basicwgt/12, na.rm=TRUE)) %>% 
  #Create a state label for a clean merge
  mutate(state='US')

#Tabulate educational attainment of US labor force 
us_educ <- cps %>% 
  #keep those in labor force only
  filter(labforce==1) %>% 
  mutate(educ2 = to_factor(educ2)) %>% 
  group_by(year, educ2) %>% 
  summarize(attain = sum(basicwgt/12, na.rm=TRUE)) %>%
  #Calculate shares
  mutate(share = attain/sum(attain)) %>% 
  pivot_wider(id_cols = year, names_from = educ2, values_from = c(attain, share)) %>% 
  #Merge US labor force data frame
  left_join(us_labforce) %>% 
  relocate(state,total_labor_force, .after=year)


################ State tabulations ################ 

#Data frame tabulating size of labor force, by state

state_labforce <- cps %>% 
  filter(labforce==1) %>% 
  group_by(year, state) %>% 
  #sum total labor force
  summarize(total_labor_force = sum(basicwgt/12, na.rm=TRUE))
         
#Tabulate educational attainment of US labor force, by state
state_educ <- cps %>% 
  #keep those in labor force only
  filter(labforce==1) %>% 
  mutate(educ2 = to_factor(educ2)) %>% 
  group_by(year, state, educ2) %>% 
  summarize(attain = sum(basicwgt/12, na.rm=TRUE)) %>%
  #Calculate shares
  mutate(share = attain/sum(attain)) %>% 
  pivot_wider(id_cols = c(year, state), names_from = educ2, values_from = c(attain, share)) %>% 
  left_join(state_labforce) %>% 
  relocate(total_labor_force, .after=state) 


################ 2022 education shares  ################ 

all_shares<- bind_rows(us_educ, state_educ) %>% 
  filter(year==2022) %>% 
  write_csv(here('output/laborforce_educ.csv'))
