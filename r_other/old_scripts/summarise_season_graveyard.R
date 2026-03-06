
# HELPFUL CODE GRAVEYARD --------------------------------------------------

# calculate how many days since last survey 

# filter to repeat surveys per site / season
# data |>
#   dplyr::group_by(ae_zone, subsite, season, year_adj, crop_type, crop_stage, date_start_session) |> # trap_type
#   mutate(count = n(),
#          fini = date_start_session, 
#          fend = lead(date_end_session), 
#          deltaD = as.numeric(fend - fini)) %>%
#   #na.omit() %>%
#   filter(count > 1) %>%
#   ungroup() %>%
#   arrange(ae_zone, subsite, season, year_adj)





# RAPID ASSESMENT DATA: HANDLE REPEAT SITES SURVEYS IN A SEASON WITH DIFFERENT CROPS ------------------------------------------------

# clean workspace 
#rm(list=setdiff(ls(), c("data_traps", "data_traps_adj",  
#                        "data_chewcards", "data_chewcards_adj",
#                        "data_burrows", "data_burrows_adj")))



# check for duplicates
#dupes <- data_chewcards_adj |>
#  dplyr::group_by(series, season, year_adj) |> 
#  mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
#  ungroup() |> 
#  arrange(series, date_start_session) |> 
#  filter(duplicate_season == "yes")




## adjust seasons when surveys occured on the cusp 
## many more than traps, so need to automate with a function:
#adjust_seasons <- function(data) {
#  
#  ## add column identifying duplicate season
#  data <- data |>
#    dplyr::group_by(series, season, year_adj) |> 
#    mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
#    ungroup()
#  # check out how many 
#  print(paste0("duplicates before:", table(data$duplicate_season)))
#  
#  # change surveys conducted on-the-cusp of seasons that have duplicates
#  # summer / autumn
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-02-15")), ymd(paste0(year(data$date_end_session),"-02-28"))), factor("MAM", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-03-01")), ymd(paste0(year(data$date_end_session),"-03-15"))), factor("Summer", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  # autumn / winter
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-05-15")), ymd(paste0(year(data$date_end_session),"-05-31"))), factor("JJA", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-06-01")), ymd(paste0(year(data$date_end_session),"-06-15"))), factor("MAM", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  # winter / spring
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-08-15")), ymd(paste0(year(data$date_end_session),"-08-31"))), factor("SON", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-09-01")), ymd(paste0(year(data$date_end_session),"-09-15"))), factor("JJA", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  # spring / summer
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-11-15")), ymd(paste0(year(data$date_end_session),"-11-30"))), factor("Summer", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-12-01")), ymd(paste0(year(data$date_end_session),"-12-15"))), factor("SON", levels = c("Summer", "MAM", "JJA", "SON"), ordered = TRUE), data$season)
#  
#  # calculate dupes again 
#  data <- data |>
#    dplyr::group_by(series, season, year_adj) |> 
#    mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
#    ungroup()
#  # check out what changed 
#  print(paste0("duplicates after:", table(data$duplicate_season)))
#  
#  # remove data and duplicate columns 
#  # data <- dplyr::select(data, !(c(duplicate_season, date_start_session, date_end_session)))
#  
#  return(data)
#}
#
## use the function
#data_burrows_adj2 <- adjust_seasons(data_burrows_adj2)
#data_chewcards_adj2 <- adjust_seasons(data_chewcards_adj2)
#
#
#
## CHECK DUPLICATES AFTER ADJUSTING SEASONS --------------------------------
#
#data_burrows_adj_dupes <- data_burrows_adj |>
#  dplyr::group_by(series, season, year_adj) |> 
#  mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
#  ungroup() |>
#  arrange(series, season, year_adj) |>
#  filter(duplicate_season == "yes")
#
#data_chewcards_adj_dupes <- data_chewcards_adj |>
#  dplyr::group_by(series, season, year_adj) |> 
#  mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
#  ungroup() |>
#  arrange(series, season, year_adj) |>
#  filter(duplicate_season == "yes")





# data_new <- data |>
#       dplyr::group_by(series, season, year_adj) |> 
#       dplyr::filter(!(row_number() > 1)) |> 
#       ungroup()
#     # print number of rows removed 
#     print(paste0("Removed ", nrow(data) - nrow(data_new), " duplicates."))


## TAKE ONLY FIRST SURVEY
## there is probably a better way to deal with these - but for now, keep the first and remove the remaining duplicates
#filter_remaining_dupes <- function(data){
#  data_new <- data |>
#    dplyr::group_by(series, season, year_adj) |> 
#    dplyr::filter(!(row_number() > 1)) |> 
#    ungroup()
#  # print number of rows removed 
#  print(paste0("Removed ", nrow(data) - nrow(data_new), " duplicates."))
#  # return filtered data 
#  return(data_new)
#}
#
## use the function on a list
#data_list_adj <- lapply(data_list_adj, filter_remaining_dupes)


