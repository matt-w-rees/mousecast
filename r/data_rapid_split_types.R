# Split summarised rapid assessment data into separate burrow and chewcard dataframes.
# Returns a named list with $burrows and $chewcards, each containing shared columns
# plus their respective survey-specific columns.

data_rapid_split_types <- function(data) {

  # Split datasets and filter specific to that data type
  data_burrows <- data |>
    select(-matches("^chewcard")) |>
    distinct() |>
    filter(burrow_transects_searched >= 1) |>
    filter(!(grepl("Values 0-25 are the total number of active burrows lumped.*", comments)))  # additional filtering of incorrect data

  data_chewcards <- data |>
    select(-matches("^(active_burrows_t|burrow_)")) |>
    filter(chewcards_deployed >= 1) |>
    distinct()

  # return as list
  return(list(
    burrows = data_burrows,
    chewcards = data_chewcards
  ))
}
