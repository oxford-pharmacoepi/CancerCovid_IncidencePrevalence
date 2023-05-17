cdm$nb_cancer_covid_cancers_3_time_periods %>% 
  rename("person_id" = subject_id) %>%
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  rename(subject_id = person_id) %>%
  group_by(cohort_definition_id, region) %>% 
  tally() %>% 
  arrange(cohort_definition_id) %>%
  collect()
