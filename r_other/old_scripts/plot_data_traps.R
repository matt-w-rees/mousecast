plot_data_traps <- function(data_list){
  
  # select trap data out of the list
  data_traps <- data_list[[1]]
  
  # plotting function for mice caught 
  plot_traps <- function(data_traps){
    plot <- data_traps %>%
      pivot_longer(., cols = c(mice_night1, mice_night2, mice_night3, mice_night4), names_to = "night", values_to = "mice_caught_night") %>%
      ggplot(aes(x = ymd(date_start_session), y = mice_caught_night, col = night, group = night)) +
      geom_line(color="grey30") + 
      facet_wrap(~subsite) +
      geom_point() +  
      xlab("") + 
      ylab("Mice caught") + 
      ggtitle(label = paste0(data_traps$ae_zone[1]))
    return(plot)
  }
  
  # store plot for each unique ae_zone and combine in a list
  plot_list_traps <- purrr::map(unique(data_traps$ae_zone),
                          function(x) {
                            # filter data before passing it to the plot function
                            data_traps %>% 
                              dplyr::filter(ae_zone == x) %>%
                              plot_traps()
                          }
  )
  
  # save plots as png 
  for(i in 1:length(plot_list_traps)){
    suppressMessages(suppressWarnings(
      ggsave(plot = plot_list_traps[[i]], width = 14, height = 6, units = "in", file = paste("derived_data/data_vis/traps/traps_ae_zone", i, ".png", sep=""))
      ))
  }
  
  return(plot_list_traps)
}