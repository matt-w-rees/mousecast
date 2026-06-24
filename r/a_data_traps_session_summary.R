data_traps_session_summary <- function(data) {
  
  # Add session-level derived variables and summaries for individuals (before removing individual variable columns in next function)
data %>%
    
  # group by farm / session  
  group_by(region_name, site_name, subsite_name, session_start_date, session_end_date) %>% #  -- note that date_start_session is calculated on a subsite level - data start / ends are not always consistent!
  
  # add vars
  mutate(
    
    # Sex ratio (count only first captures to avoid double-counting recaptures)
    n_males_site = sum(sex == "male" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE),
    n_females_site = sum(sex == "female" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE),
    number_unique_individuals_session = n_males_site + n_females_site
    # see r_not_in_use/breeding_sign_indicators.R for an abandoned draft of
    # sex-ratio-proportion and breeding-sign columns that could go here
  ) %>%
  ungroup()
}
