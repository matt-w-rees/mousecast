tar_load(data_ecology_raw)

# EAR NOTCHES --------------------------------------------------------------

# check for ear notches not being entered correctly
filtered_df <- data_ecology_raw %>% filter(if_any(everything(), ~grepl("notch", .)))
filtered_df2 <- data_ecology_raw %>% filter(if_any(everything(), ~grepl("Notch", .)))
ear_notches_ecology <- bind_rows(filtered_df, filtered_df2)
write.csv(ear_notches_ecology, "ecology_errors/ear_notches_ecology.csv")


# take individual mice with pit tags, and summarise captures, movements, survival
tagged <- filter(data_ecology_raw, !(is.na(pit_tag_id))) |>
  # order all records chronologically
  arrange(date) |>
  # add summary columns
  group_by(pit_tag_id) |>
  mutate(
    n_captures = n(),
    n_sites = n_distinct(site),
    n_regions = n_distinct(region),
    n_sexes = length(unique(sex[!is.na(sex)])),
    sites = paste(unique(site), collapse = " -> "),
    regions = paste(unique(region), collapse = " -> "),
    first_date = min(date),
    last_date = max(date),
    date_range_days = as.numeric(difftime(max(date), min(date), units = "days")),
    sessions = paste(unique(session), collapse = ", ")
    ) |>
  ungroup()




# CHECK CLASS -------------------------------------------------------------

## take the first capture of the individual (as we have ordered by date), and make sure it is the first_capture
not_first_capture <- tagged |>
  group_by(pit_tag_id) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(class != "first_capture")

# save records
write.csv(not_first_capture, "ecology_errors/earliest_record_of_individual_but_not_marked_as_first_capture.csv")

## also other way around, remove the first instance and check its not first capture
subsequent_capture_marked_as_first <- tagged |>
  group_by(pit_tag_id) |>
  slice(-1) |>
  ungroup() |>
  filter(class == "first_capture")

# save records
write.csv(subsequent_capture_marked_as_first, "ecology_errors/subsequent_capture_recorded_as_first_captures.csv")



# UNLIKELY BIOLOGY ---------------------------------------------------------------
moved_bw_regions <- tagged |>
  filter(n_regions > 1)
# none

moved_bw_sites <- tagged |>
  filter(n_sites > 1) |>
  group_split(pit_tag_id) |>
  imap(~write_csv(.x, paste0('ecology_errors/site_movements/', .y, '.csv')))
# save a spreadsheet for each group



# SEX ---------------------------------------------------------------------

# no sex 
no_sex <- tagged |>
  filter(n_sexes == 0)
write_csv(no_sex, 'ecology_errors/no_sex.csv')

# two sex
two_sex <- tagged |>
  filter(n_sexes == 2) |>
  group_split(pit_tag_id) |>
  imap(~write_csv(.x, paste0('ecology_errors/two_sex/', .y, '.csv')))






