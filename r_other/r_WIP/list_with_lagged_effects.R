list_with_lagged_effects <- function(data){
  
  # see - https://ecogambler.netlify.app/blog/distributed-lags-mgcv/ 
  # and - https://rpubs.com/NickClark47/mvgam3
  # for info and where this code was derived
  
  # 48 month lag period chosen - this also needs to be specified in the 'summarise_traps_by_month' function (L114)
  
  # Simon Wood’s lag matrix function (which he uses in his distributed lag example from his book Generalized Additive Models - An Introduction with R 2nd edition).
  lagard <- function(x, n_lag = 6) {
    n <- length(x)
    X <- matrix(NA, n, n_lag)
    for (i in 1:n_lag){
      X[i:n, i] <- x[i:n - i + 1]
    } 
    X
  }
  
  # We can make use of this function to organise all data needed for modeling into a list. For this simple example we will use lags of up to six months in the past. Some bookkeeping is necessary, as we must ensure that we remove the first five observations for each series. This is best achieved by arranging the data by series and then by time. I do this first to create the mintemp matrix
  precip <- do.call(rbind, lapply(seq_along(levels(data$series)), function(x){
    data %>%
      dplyr::filter(series == levels(data$series)[x]) %>%
      dplyr::arrange(time) %>%
      dplyr::select(precip, time) %>%
      dplyr::pull(precip) -> tempdat
    
    lag_mat <- lagard(tempdat, 48)
    tail(lag_mat, NROW(lag_mat) - 47)
  }))
  dim(precip)
  
  # Now we can arrange the data in the same way and pull necessary objects into the list:
  data %>%
    dplyr::arrange(series, time) %>%
    dplyr::filter(time > 47) -> data
  
  ## make a list for distributed lag models - specify max lag period
  data_list <- list(
    lag = matrix(1:48, nrow(data), 48, byrow = TRUE),
    series = data$series,
    time = data$time,
    year_month = data$year_month,
    mice_month = data$mice_month,
    trapnights_month = data$trapnights_month,
    latitude = data$latitude,
    longitude = data$longitude,
    ae_zone = data$ae_zone,
    month = data$month,
    sm_rz_pct_6m_6m = data$sm_rz_pct_6m,
    precip_prev_0_18m = data$precip_prev_0_18m,
    precip_prev_19_26m = data$precip_prev_19_26m,
    tmax = data$tmax,
    tmin = data$tmin
  )
  dim(data_list$lag)
  
  # and add lagged precip in 
  data_list$precip <- precip
  
  
  ## old code!  
  # add lagged effects
  #data_list$precip <- lagard(data$precip)
  #data_list$tmax <- lagard(data$tmax)
  #data_list$tmin <- lagard(data$tmin)
  #data_list$sm_rz_pct_6m <- lagard(data$sm_rz_pct_6m)
  
  # remove first x rows due to NA's
  #data_list <- lapply(data_list, tail, - 48)
  
  
  # the best way I've found to do this, which is unfortunately a bit cumbersone:
  # create weight matrices for each level of the grouping factor for setting up hierarchical 
  # distributed lag terms
  weights_s1 <- weights_s2 <- 
    weights_s3 <- weights_s4 <-
    weights_s5 <- weights_s6 <- 
    weights_s7 <- weights_s8 <-
    weights_s9 <- matrix(1, ncol = ncol(data_list$lag), 
                                       nrow = nrow(data_list$lag))
  
  # for each series, the rows in its weighting matrix need to have '1s' when the corresponding
  # observation in data matches that series, and '0s' otherwise
  weights_s1[!(data_list$series == 'adelaide plains'), ] <- 0
  data_list$weights_s1 <- weights_s1
  
  weights_s2[!(data_list$series == 'callide valley'), ] <- 0
  data_list$weights_s2 <- weights_s2
  
  weights_s3[!(data_list$series == 'central west nsw'), ] <- 0
  data_list$weights_s3 <- weights_s3
  
  weights_s4[!(data_list$series == 'coleambally'), ] <- 0
  data_list$weights_s4 <- weights_s4
  
  weights_s5[!(data_list$series == 'dawson valley'), ] <- 0
  data_list$weights_s5 <- weights_s5
  
  weights_s6[!(data_list$series == 'downs central'), ] <- 0
  data_list$weights_s6 <- weights_s6
  
  weights_s7[!(data_list$series == 'eastern downs'), ] <- 0
  data_list$weights_s7 <- weights_s7
  
  weights_s8[!(data_list$series == 'northern downs'), ] <- 0
  data_list$weights_s8 <- weights_s8

  weights_s9[!(data_list$series == 'northern mallee'), ] <- 0
  data_list$weights_s9 <- weights_s9
    
  return(data_list)
}