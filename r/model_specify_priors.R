model_specify_priors <- function(){

  priors <- c(
    
    
    ## Observation model: error / dispersion: phi
    #--> think of it as variability around the true population size (therefore needs to a positive number)
    # for the gamma distribution, the first number controls the shape (alpha) and the second is rate (beta); mean = alpha/beta, variance = alpha/beta^2
    # gamma(4, 1): mean = 4, variance = 4 — concentrates around the observed posterior medians of 2–4 while allowing higher values
    prior("gamma(4, 1)", class = "phi"),

    ## Process model: 
    

    
    
    # --- AR(1) parameters (latent trend persistence) -----------------
    # Normal(0.3, 0.35) encourages moderate positive autocorrelation
    # but the wider SD (0.35 vs 0.2) allows the data to have more influence,
    # accommodating series where persistence is weaker or near zero.
    # The Stan declaration already constrains ar1 ∈ [-1, 1].
    # default prior: ar1 ~ std_normal()
    prior("normal(0.3, 0.35)", class = "ar1"),

    # --- Process innovation SD (sigma) -------------------------------
    # Student-t with 3 df, centred at 0, scale = 0.25.
    # Half-Student-t (positive only) favours modest state noise
    # while allowing heavier tails (occasional larger shocks).
    # Keeps the latent trend from absorbing all variability.
    prior("student_t(3, 0, 0.25)", class = "sigma")

    # --- Beta regression precision (phi) ------------------------------
    # Gamma(2, 0.5) prior has mean = 4 and variance = 8.
    # This favours moderate precision (i.e. avoids extreme over/under 
    # dispersion in the Beta observation model) while staying flexible.
   # prior("gamma(2, 0.5)", class = "phi"), 
    
    # --- Spline smoothness penalties for rainfall lag surfaces --------
    # Exponential(1) prior on smoothing penalties shrinks towards 
    # smoother, simpler functions (penalising wiggliness), but still 
    # permits more complex shapes when data strongly support them.
    #prior("exponential(1)", class = "lambda_trend"),
    
    ## Survey method / crop stage covariate for observation model
    # remove the intercept (-1) for the observation model and specify each survey_method_adj category seperately 
    # Informative priors for survey_method_adj (absolute effects, logit scale)
    # burrows_old:       p ≈ 0.05 (5%) → logit ≈ -2.94  (i.e., on average, 5 / 100 transects would detect at least one active burrow)
    # burrows_young:     p ≈ 0.13 (13%) → logit ≈ -1.90 (i.e., on average, 13 / 100 transects would detect at least one active burrow)
    # chewcards:         p ≈ 0.08 (8%) → logit ≈ -2.44  (i.e., on average, 8 / 100 chewcards would have one sign of mice)
    # traps_night1:      p ≈ 0.05 (5%) → logit ≈ -2.94  (i.e., on average, 5 / 100 traps would catch a mouse on night 1)
    # traps_night2:      p ≈ 0.08 (8%) → logit ≈ -2.44  (i.e., on average, 8 / 100 traps would catch a mouse on night 2)
    # traps_night3:      p ≈ 0.09 (9%) → logit ≈ -2.40   (i.e., on average, 9 / 100 traps would catch a mouse on night 3)
    #prior(normal(-2.94, 0.25), class = "b", coef = "survey_method_adjburrows_old"),
    #prior(normal(-1.90, 0.25), class = "b", coef = "survey_method_adjburrows_young"),
    #prior(normal(-2.44, 0.25), class = "b", coef = "survey_method_adjchewcards"),
    #prior(normal(-2.94, 0.25), class = "b", coef = "survey_method_adjtraps_night1"),
    #prior(normal(-2.44, 0.25), class = "b", coef = "survey_method_adjtraps_night2"),
    #prior(normal(-2.40, 0.25), class = "b", coef = "survey_method_adjtraps_night3")

  
    ## Seasonal effects for process model
    # make sure factor is unordered (so we can place prior on each level distinctly - model estimates ordered factors differently)
    # and reference level is winter - this anchors it to the low abundance phase for which is frequently surveyed 
    # Winter (Intercept):  logit = -2.94 → probability ≈ 5%
    # Spring vs Winter:    +0.00         → probability ≈ 5%
    # Summer vs Winter:    +0.95         → probability ≈ 12%
    # Autumn vs Winter:    +1.55         → probability ≈ 20%
    #prior(normal(-2.94, 0.5), class = "b", coef = "(Intercept)_trend"),  # Winter baseline (~5% detectability), much less certainty about where this falls (relative to certainty about the relative seasonal effects)
    #prior(normal(0.00, 0.25), class = "b", coef = "seasonSpring"),
    #prior(normal(0.95, 0.25), class = "b", coef = "seasonSummer"),
    #prior(normal(1.55, 0.25), class = "b", coef = "seasonAutumn")
    
    # positive prior mean
    #zprior(normal(0.5, 0.25), class = "b", coef = "s(rainfall_lag)"),
    #zprior(normal(0.5, 0.25), class = "b", coef = "(evapotranspiration_lag)"),
    #zprior(normal(0.5, 0.25), class = "b", coef = "(soil_moisture_lag)"),
    #zprior(normal(0.5, 0.25), class = "b", coef = "(degree_days_lag)"),
    
    # negative prior mean
    #prior(normal(-0.5, 0.25), class = "b", coef = "s(degree_days_lag)"),
    #prior(normal(-0.5, 0.25), class = "b", coef = "s(heat_days_lag)")
    
  )
  
  return(priors)

}

