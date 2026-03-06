# plot forecasts at benchmark sites

plot_forecasts <- function(model, outdir){
  
  #model <- readRDS("derived_data/model_fits/dynamic/trend/rainfall_lag.RDS")
  #model <- readRDS("derived_data/model_fits/dynamic/no_trend/fit_rainfall.RDS")
  
  
  # Central West NSW --------------------------------------------------------
  png(paste0(outdir, "forecast_NSW.png"), width = 9, height = 14, res = 500, units = "in")
  par(mfrow = c(5, 2))
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 7, main = "Site 1: traps night 1")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 12, main = "Site 2: traps night 1")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 8, main = "Site 1: traps night 2")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 13, main = "Site 2: traps night 2")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 9, main = "Site 1: traps night 3")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 14, main = "Site 2: traps night 3")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 10, main = "Site 1: burrows")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 15, main = "Site 2: burrows")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 11, main = "Site 1: chewcards")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 16, main = "Site 2: chewcards")
  dev.off()
  
  # also check the overall trends
  #plot(model, type = "trend", ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 9)
  #plot(model, type = "trend", ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 14)
  
  
  # VIC MALLEE -------------------------------------------------
  png(paste0(outdir, "forecast_VIC.png"), width = 9, height = 14, res = 500, units = "in")
  par(mfrow = c(5, 2))
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 211, main = "Site 1: traps night 1")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 216, main = "Site 2: traps night 1")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 212, main = "Site 1: traps night 2")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 217, main = "Site 2: traps night 2")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 213, main = "Site 1: traps night 3")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 218, main = "Site 2: traps night 3")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 214, main = "Site 1: burrows")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 219, main = "Site 2: burrows")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 215, main = "Site 1: chewcards")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 220, main = "Site 2: chewcards")
  dev.off()
  
  # also check the overall trends
  #plot(model, type = "trend", ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 301)
  #plot(model, type = "trend", ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 306)
  
  
  # SOUTH AUSTRALIA MALLALA -------------------------------------------------
  png(paste0(outdir, "forecast_SA.png"), width = 9, height = 14, res = 500, units = "in")
  par(mfrow = c(5, 2))
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 299, main = "Site 1: traps night 1")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 304, main = "Site 2: traps night 1")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 300, main = "Site 1: traps night 2")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 305, main = "Site 2: traps night 2")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 301, main = "Site 1: traps night 3")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 306, main = "Site 2: traps night 3")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 302, main = "Site 1: burrows")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 307, main = "Site 2: burrows")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 303, main = "Site 1: chewcards")
  plot(model, type = "forecast", ylim = c(0,1), ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 308, main = "Site 2: chewcards")
  dev.off()
  
  # also check the overall trends
  #plot(model, type = "trend", ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 301)
  #plot(model, type = "trend", ylab = "Abundance", xlab = "Timestep (2013 - 2025)", series = 306)

}