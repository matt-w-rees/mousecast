plot_data_burrows <- function(data_list){
  
  # select rapid assessment data out of the list
  data_burrows <- data_list[[2]]
  
  # plotting function for proportion cards with chews 
  plot_burrows <- function(data_burrows){
    data_burrows %>%
      ggplot(aes(x = ymd(date_start_session), y = burrow_total)) +
      geom_point() +
      geom_line(color="grey30") + 
      facet_wrap(~subsite, ncol = 4) + 
      xlab("") + 
      ylab("Active burrow count") + 
      ggtitle(label = paste0(unique(data_burrows$ae_zone)))
  }
  
  # store plot for each unique ae_zone and combine in a list
  plot_list_burrows <- purrr::map(unique(data_burrows$ae_zone),
                          function(x) {
                            # filter data before passing it to the plot function
                            data_burrows %>% 
                              dplyr::filter(ae_zone == x) %>%
                              plot_burrows()
                          }
  )
  
  # save plots as png 
  for(i in 1:length(plot_list_burrows)){
    suppressMessages(suppressWarnings(
      ggsave(plot = plot_list_burrows[[i]], width = 14, height = 6, units = "in", file = paste("derived_data/data_vis/burrows/burrows_ae_zone", i, ".png", sep=""))
    ))
  }
  
  return(plot_list_burrows)
}