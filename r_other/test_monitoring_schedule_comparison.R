# Test script to compare monitoring schedule with actual survey data
# This script demonstrates how to use the compare_monitoring_schedule() function

# Load required packages
library(targets)
library(tidyverse)

# Source the function
source("r/compare_monitoring_schedule.R")

# Load the data target
tar_load(data)

# Run the comparison - returns a single dataframe with one row per region per season-year
# This will also produce a message listing leads that have not met expectations by season-year
comparison_df <- compare_monitoring_schedule(
  data_list = data,
  schedule_path = "raw_data/monitoring_schedule.csv",
  start_season_year = "Autumn-2025",
  end_season_year = "Spring-2029"
)

# View the results
print(comparison_df, n = 50)

# Example analyses:

# Summary by lead (total across all years/seasons/regions)
cat("\n=== Summary by Lead ===\n")
comparison_df |>
  group_by(lead) |>
  summarise(
    n_regions = n_distinct(region),
    total_expected = sum(expected_sites),
    burrows_total = sum(burrows_sites),
    burrows_shortfall = sum(burrows_difference[burrows_difference < 0]),
    chewcards_total = sum(chewcards_sites),
    chewcards_shortfall = sum(chewcards_difference[chewcards_difference < 0]),
    .groups = "drop"
  ) |>
  print()

# Summary by region (total across all years/seasons)
cat("\n=== Summary by Region ===\n")
comparison_df |>
  group_by(lead, region) |>
  summarise(
    total_expected = sum(expected_sites),
    burrows_total = sum(burrows_sites),
    burrows_shortfall = sum(burrows_difference[burrows_difference < 0]),
    chewcards_total = sum(chewcards_sites),
    chewcards_shortfall = sum(chewcards_difference[chewcards_difference < 0]),
    .groups = "drop"
  ) |>
  print(n = Inf)

# View most recent season-year for each lead/region
cat("\n=== Most Recent Season-Year by Lead/Region ===\n")
comparison_df |>
  group_by(lead, region) |>
  slice_max(season_year_adj, n = 1) |>
  select(lead, region, season_year_adj, expected_sites,
         burrows_sites, burrows_difference, chewcards_sites, chewcards_difference) |>
  print(n = 30)

# View a specific lead
cat("\n=== CSIRO Detail (first 20 rows) ===\n")
comparison_df |>
  filter(lead == "CSIRO") |>
  print(n = 20)

# Save to CSV
# write.csv(comparison_df,
#           "derived_data/monitoring_schedule_comparison.csv",
#           row.names = FALSE)
