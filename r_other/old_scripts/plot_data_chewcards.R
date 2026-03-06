plot_data_chewcards <- function(data_list){
  
  # select chewcard data out of the list
  data_chewcards <- data_list[[3]]
  
  # add summary columns to dataframe
  data_chewcards <- data_chewcards %>%
    mutate(chewcard_present = apply(select(., contains("chewcard.")), 1, function(i) sum(i > 0, na.rm = TRUE)),                          # NOTE returns 0's instead of NA's - fix is below; total number of chewcards with any sign of mice chew: 0 - 10:
           chewcard_sum = apply(select(., contains("chewcard.")), FUN = sum, MARGIN = 1, na.rm = TRUE), 
           chewcard_mean = apply(select(., contains("chewcard.")), FUN = mean, MARGIN = 1, na.rm = TRUE))
  
  
  # plotting function for proportion cards with chews 
  plot_chewcards <- function(data_chewcards){
    data_chewcards %>%
      ggplot(aes(x = ymd(date_start_session), y = chewcard_present / chewcards_deployed)) +
      scale_y_continuous(limits = c(0, 1)) + 
      geom_point() +
      geom_line(color="grey30") + 
      facet_wrap(~subsite, ncol = 4) + 
      xlab("") + 
      ylab("Proportion cards chewed") + 
      ggtitle(label = paste0(unique(data_chewcards$ae_zone)))
  }
  
  # store plot for each unique ae_zone and combine in a list
  plot_list_chewcards <- purrr::map(unique(data_chewcards$ae_zone),
                          function(x) {
                            # filter data before passing it to the plot function
                            data_chewcards %>% 
                              dplyr::filter(ae_zone == x) %>%
                              plot_chewcards()
                          }
  )
  
  # save plots as png 
  for(i in 1:length(plot_list_chewcards)){
    suppressMessages(suppressWarnings(
      ggsave(plot = plot_list_chewcards[[i]], width = 14, height = 6, units = "in", file = paste("derived_data/data_vis/chewcards/chewcards_ae_zone", i, ".png", sep=""))
    ))
  }
  
  return(plot_list_chewcards)
}