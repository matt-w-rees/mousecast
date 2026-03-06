data_traps_session_summary <- function(data) {
  
  # Add session-level derived variables and summaries for individuals (before removing individual variable columns in next function)
data %>%
    
  # group by farm / session  
  group_by(region, site, subsite, session_start_date, session_end_date) %>% #  -- note that date_start_session is calculated on a subsite level - data start / ends are not always consistent!
  
  # add vars
  mutate(
    
    # Sex ratio (count only first captures to avoid double-counting recaptures)
    n_males_site = sum(sex == "male" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE),
    n_females_site = sum(sex == "female" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE),
    number_unique_individuals_session = n_males_site + n_females_site
    #sex_ratio_prop_male_site = if_else(n_unique_individuals_site > 0, n_males_site / n_unique_individuals_site, NA_real_),
    
    # Breeding indicators (count only first captures to avoid double-counting recaptures)
    # Definitive sign: multiple pregnant females (first captures only)
   # breeding_sign_preg_site = if_else(
   #   sum(pregnant == "yes" & class == "first_capture", na.rm = TRUE) > 1, 1, 0
   # ),
    # 2nd most definitive sign: any perforate vagina
   # breeding_sign_vag_site = if_else(
   #   any(vagina %in% c("perforate_small", "perforate_large"), na.rm = TRUE), 1, 0
   #  ),
   # 3rd most definitive sign: any enlarged teats
   # breeding_sign_teats_site = if_else(
   #   any(teats == "present_large_fur_not_at_base", na.rm = TRUE), 1, 0
    #)
  ) %>%
  ungroup()
}
