data_model_set_response_variable <- function(data_list){

  data_list |>

  ## Rename relevant response variable to be the same each for dataframe
  # success variable: number of (a) mice caught, (b) transects with active burrows, (c) chewcards with mice sign
  purrr::map2(.y = list("number_mice_caught", "burrow_transects_present", "chewcards_chewed"),
            ~ rename(.x, !!"n_success" := all_of(.y))) |>
  # trials variable: number of (a) traps deployed, (b) transects searched, (c) chewcards deployed
  purrr::map2(.y = list("number_functional_traps", "burrow_transects_searched", "chewcards_deployed"),
              ~ rename(.x, !!"n_trial" := all_of(.y))) |>


  # Now add new proportion variable (to be used as the response variable)
  purrr::map(~ .x |> mutate(
    # Where n_success exceeds n_trial (more mice caught than functional traps set),
    # cap n_success at n_trial so the proportion stays <= 1. This is rare and only
    # occurs in trap data; rapid assessment data is not affected.
    n_success = pmin(n_success, n_trial, na.rm = TRUE),
    # Compute proportion and apply Beta bounds [0.001, 0.999]: {mvgam} does not yet
    # support zero- or one-inflated Beta observations so exact zeros and ones are nudged
    # inward by a small offset.
    mice_prop = pmin(0.999, pmax(0.001, n_success / n_trial))
  ))

}
