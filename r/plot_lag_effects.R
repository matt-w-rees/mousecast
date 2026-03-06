library(dplyr)
library(ggplot2)
library(viridis)

# Plot distributed lag effects of rainfall deviations
# 
# This function computes predictions from a fitted mvgam model to visualize:
#   - The average effect of a predictor over lag periods
#   - Zone-specific deviations (hierarchical distributed lag)
# Curves are centered within each (zone × lag) combination.

  plot_lag_effects <- function(model_file,
                               model_data_lag,
                               smooth_var,
                               lag_var = "lag",
                               outdir){
    # ---- Packages ----
    stopifnot(requireNamespace("dplyr", quietly = TRUE))
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    stopifnot(requireNamespace("viridis", quietly = TRUE))
    stopifnot(requireNamespace("gganimate", quietly = TRUE))
    stopifnot(requireNamespace("mgcv", quietly = TRUE))
    
    # ---- Load model ----
    model <- readRDS(model_file)
    
    # specify smooth variable to plot based on file path name
    smooth_var <- sub(".*fit_(.*)\\.RDS", "\\1", model_file)
    

    # ---- 1) Build the prediction grid ----
    lag_values <- sort(unique(model_data_lag[[lag_var]]))
    smooth_values <- seq(min(model_data_lag[[smooth_var]], na.rm = TRUE),
                         max(model_data_lag[[smooth_var]], na.rm = TRUE),
                         length.out = 100)
    
    # create a new version of data as a data.frame so it's easier to work with
    model_data <- as.data.frame(model_data_lag)
    
    n_lags <- length(lag_values)
    n_x    <- length(smooth_values)
    
    # Base expand.grid with PROGRAMMATIC names
    grid_list <- setNames(list(lag_values, smooth_values), c(lag_var, smooth_var))
    base_grid <- do.call(expand.grid, c(grid_list,
                                        list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
    
    # ---- 2) Add all required columns/constants to new_data ----
    weight_cols  <- grep("^weights_", names(model_data), value = TRUE)
    required_vars <- setdiff(names(model_data), c(lag_var, smooth_var))
    
    template <- model_data[1, required_vars, drop = FALSE]
    
    # Keep factor levels intact for non-weight vars
    for (nm in names(template)) {
      if (is.factor(model_data[[nm]])) {
        template[[nm]] <- factor(template[[nm]], levels = levels(model_data[[nm]]))
      }
    }
    
    # Repeat template across the grid
    new_data <- base_grid
    for (nm in names(template)) {
      new_data[[nm]] <- template[[nm]][1]
    }
    
    # Ensure weights are numeric and default to 0
    for (w in weight_cols) {
      new_data[[w]] <- 0
    }
    
    # ---- 3) Average effect ----
    te_pat <- paste0("te\\(", smooth_var, ",", lag_var, "\\)$")
    pr <- predict(model, newdata = new_data, type = "terms", se.fit = TRUE)
    proc_fit <- pr$process_effects[[1]]
    proc_se  <- pr$process_effects[[2]]
    te_avg_name <- grep(te_pat, colnames(proc_fit), value = TRUE)
    
    avg_df <- data.frame(
      lag   = new_data[[lag_var]],
      x     = new_data[[smooth_var]],
      effect = proc_fit[, te_avg_name],
      se     = proc_se[, te_avg_name],
      group  = "Average",
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(lower = effect - 1.96 * se,
                    upper = effect + 1.96 * se)
    
    # ---- 4) Zone-specific deviations ----
    zone_effects <- list()
    for (zone in weight_cols) {
      zd <- new_data
      zd[weight_cols] <- 0   # reset all weights to 0
      zd[[zone]] <- 1        # set focal weight to 1
      
      pr_zone <- predict(model, newdata = zd, type = "terms", se.fit = TRUE)
      z_fit <- pr_zone$process_effects[[1]]
      z_se  <- pr_zone$process_effects[[2]]
      
      # look for interaction terms with the numeric weight
      te_zone <- grep(paste0("te\\(", smooth_var, ",", lag_var, "\\):", zone), 
                      colnames(z_fit), value = TRUE)
      
      if (length(te_zone) > 0L) {
        z_effect <- rowSums(z_fit[, te_zone, drop = FALSE])
        z_sefit  <- sqrt(rowSums(z_se[, te_zone, drop = FALSE]^2))
      } else next
      
      zdf <- data.frame(
        lag    = zd[[lag_var]],
        x      = zd[[smooth_var]],
        effect = z_effect + avg_df$effect,
        se     = z_sefit,
        group  = zone,
        stringsAsFactors = FALSE
      ) |>
        dplyr::mutate(lower = effect - 1.96 * se,
                      upper = effect + 1.96 * se)
      
      zone_effects[[zone]] <- zdf
    }
    
    # ---- 5) Combine Average + zones and center per (group × lag) ----
    all_effects <- dplyr::bind_rows(
      avg_df,
      if (length(zone_effects)) dplyr::bind_rows(zone_effects) else NULL
    )
    
    all_effects <- all_effects |>
      dplyr::group_by(group, lag) |>
      dplyr::mutate(
        effect_centered = effect - mean(effect, na.rm = TRUE),
        lower_centered  = lower - mean(effect, na.rm = TRUE),
        upper_centered  = upper - mean(effect, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
    
    # ---- 6) Reformat data for plotting ----
    all_effects <- all_effects |>
      dplyr::mutate(lag = factor(lag),
                    group = gsub("weights_", "", group))
    
    # ---- 7) Animated plot ----
    p_anim <- ggplot2::ggplot(all_effects, ggplot2::aes(x = x, y = effect_centered,
                                                        colour = lag, fill = lag)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::facet_wrap(~ group, scales = "fixed") +
      viridis::scale_color_viridis(discrete = TRUE) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggplot2::labs(
        x = smooth_var,
        y = "Centered effect",
        colour = "Lag", fill = "Lag",
        title = "{closest_state} seasons prior"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
      gganimate::transition_states(lag, transition_length = 0, state_length = 1) +
      gganimate::shadow_mark(alpha = 0.2, size = 0.5)
    
    # ---- 8) Save animation ----
    save_path = paste0(outdir, smooth_var, ".gif")
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    gganimate::animate(p_anim, renderer = gganimate::gifski_renderer(save_path),
                       width = 800, height = 600, fps = 4)
    
    message("Animated plot saved to: ", save_path)
    return(save_path)
  }
  