#..........................................................................................
### +++++++ ESTIMATION OF COVID-19 ATTRIBUTABLE MORTALITY IN ADEN, YEMEN (2020) +++++++ ###
#..........................................................................................

#..........................................................................................
## ----------------- R CODE TO PREPARE DATA AND FIT STATISTICAL MODELS ----------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Sep 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................                            
### Implementing approach 1: Case-based method (analysing each cemetery alone)

  #...................................   
  ## Identify analysis end date (last time point for which graves are quantified in all cemeteries)
  x1 <-  obs[complete.cases(obs[, c("graves")]), c("cemetery", "time_base", "date", "graves")]   
  date_max <- min( aggregate(x1[, "date"], by = list(x1$cemetery), FUN = max)[, 2] )
    # and also first date on which graves are quantified in all cemeteries - for baseline death rate)
    date_min <- max( aggregate(x1[, "date"], by = list(x1$cemetery), FUN = min)[, 2] )
      
  #...................................   
  ## Smooth spline method
  
    # Set up analysis output, for each candidate date
    out <- expand.grid(sort(unique(obs$cemetery)), date_knot_options )
    colnames(out) <- c("cemetery", "date_knot")
    out[, c("baseline_rate", "actual_rate", "excess_rate",
            "counterfactual_toll", "actual_toll", "excess_toll", "baseline_toll", "baseline_toll_complete",
            "days_baseline", "days_epidemic")] <- NA
  
    # Also set up dataset to store data for each cemetery
    out_fit_spline <- c()
    
    # For each cemetery...
    for (i in 1:length( sort(unique(obs$cemetery)) ) ) {
    
      # Identify cemetery
      j <- sort(unique(obs$cemetery))[i]
      
      # Prepare data
      x0 <- subset(obs, cemetery == j)
      x1 <- c("graves", "time_base", "date", "cemetery")
      x2 <- x0[complete.cases(x0[, x1]), x1]
      
      # Fit smooth spline
      x1 <- predict(smooth.spline(x2[, "time_base"], x2[, "graves"], spar = 0.2) , x0[, "time_base"] )
        
        # plot and predict across dataset for the cemetery
        actual <- f_plot_spline(x1, x0)

      # Calculate new graves every day
        actual <- actual[order(actual[, "date"]), ]
        actual[, "new_graves_actual"] <- c(0, diff(actual$predictions) )
      
      # Restrict to data availability period
        # to period of maximum data availability across all cemeteries
        actual <- subset(actual, date <= date_max)
      
        # to period of minimum data availability in this cemetery
        date_start_local <- min(x2[, "date"])
        actual <- subset(actual, date >= date_start_local)
        
        # store output
        out_fit_spline <- rbind(out_fit_spline, actual)
        
      # Calculate burial rates and tolls in different periods, by epidemic start date
      for (k in 1:length(date_knot_options) ) {
        # identify date of epidemic start
        date_knot_k <- date_knot_options[k]
        
        # reclassify  period
        actual[, "period_covid"] <- ifelse(actual$date < date_knot_k, "baseline", "epidemic")
        actual[, "period_covid"] <- as.factor(actual$period_covid)
      
        # compute cemetery statistics
        x3 <- as.integer(i + length(unique(obs$cemetery)) * (k - 1) )
        out[x3, "days_baseline"] <- as.integer(date_knot_k - date_start_local)
        out[x3, "days_epidemic"] <- as.integer(date_max - date_knot_k)
        out[x3, "baseline_toll"] <- sum(subset(actual, period_covid == "baseline")$new_graves_actual, na.rm = TRUE)
        out[x3, "baseline_toll_complete"] <- sum(subset(actual, period_covid == "baseline" & date > date_min)$new_graves_actual, na.rm = TRUE)
        out[x3, "baseline_rate"] <- out[x3, "baseline_toll"] / out[x3, "days_baseline"]
        out[x3, "actual_toll"] <- sum(subset(actual, period_covid == "epidemic")$new_graves_actual, na.rm = TRUE)
        out[x3, "actual_rate"] <- out[x3, "actual_toll"] / out[x3, "days_epidemic"]
        out[x3, "excess_rate"] <- out[x3, "actual_rate"] - out[x3, "baseline_rate"] *
          mean(subset(actual, period_covid == "epidemic")[, "increase_high"], na.rm = TRUE) /
          mean(subset(actual, period_covid == "baseline")[, "increase_high"], na.rm = TRUE)
        out[x3, "counterfactual_toll"] <- out[x3, "baseline_rate"] * out[x3, "days_epidemic"] *
          mean(subset(actual, period_covid == "epidemic")[, "increase_high"], na.rm = TRUE) /
          mean(subset(actual, period_covid == "baseline")[, "increase_high"], na.rm = TRUE)
          # rough adjustment for increasing population denominator over time
        out[x3, "excess_toll"] <- out[x3, "actual_toll"] - out[x3, "counterfactual_toll"]
      }
    }    
    
    # Format output
    out[, grep("toll", colnames(out))] <- round(out[, grep("toll", colnames(out))], digits = 0)
    out[, grep("rate", colnames(out))] <- round(out[, grep("rate", colnames(out))], digits = 2)
    
    # Save output
    write.csv(out, "out_case-based_spline.csv", row.names = FALSE)
    
    # Compute overall mortality statistics for each alternative epidemic start date
      # set up output
      out_spline <- data.frame(date_knot_options)
      colnames(out_spline) <- "epidemic_start"
      out_spline[, c("baseline_death_rate", "excess_death_rate", "SMR", "excess_toll")] <- NA
    
    for (i in 1:length(date_knot_options) ) {
      # select output for this epidemic start date
      x1 <- subset(out, date_knot == date_knot_options[i])
      # baseline death (burial) rate per 1000 person-years (only computed for period of 100% data completeness)     
      out_spline[i, "baseline_death_rate"] <- sum(x1$baseline_toll_complete, na.rm = TRUE) * 1000 * 365 / 
        (mean(subset(obs, date %in% c(date_min : date_knot_options[i]))[, "pop_high"], na.rm = TRUE) * 
         as.integer(date_knot_options[i] - date_min ) )
      # excess death toll
      out_spline[i, "excess_toll"] <- sum(x1$excess_toll, na.rm = TRUE)
      # excess death (burial) rate per 1000 person-years
      out_spline[i, "excess_death_rate"] <- out_spline[i, "excess_toll"] * 1000 * 365 /
        (mean(subset(obs, date %in% c(date_knot_options[i] : date_max))[, "pop_high"], na.rm = TRUE) *  
         as.integer(date_max - date_knot_options[i] ) )
      # standardised mortality ratio (SMR)
      out_spline[i, "SMR"] <- sum(x1$actual_toll, na.rm = TRUE) / sum(x1$counterfactual_toll, na.rm = TRUE)
      }
 
      # Visualise and save results
      out_spline[, grep("toll", colnames(out_spline))] <- round(out_spline[, grep("toll", colnames(out_spline))], digits = 0)
      out_spline[, grep("rate", colnames(out_spline))] <- round(out_spline[, grep("rate", colnames(out_spline))], digits = 2)
      out_spline[, "SMR"] <- round(out_spline[, "SMR"], digits = 2)
      print(out_spline)
      write.csv(out_spline, paste("out_est_spline_", as.character(date_max), ".csv", sep = ""), row.names = FALSE)      
        
        # also save fit raw data
        write.csv(out_fit_spline, "out_fit_spline.csv", row.names = FALSE)

            
  #...................................   
  ## Linear interpolation method
  
    # Set up analysis output, for each candidate date
    out <- expand.grid(sort(unique(obs$cemetery)), date_knot_options )
    colnames(out) <- c("cemetery", "date_knot")
    out[, c("baseline_rate", "actual_rate", "excess_rate",
            "counterfactual_toll", "actual_toll", "excess_toll", "baseline_toll", "baseline_toll_complete",
            "days_baseline", "days_epidemic")] <- NA
  
    # Also set up dataset to store data for each cemetery
    out_fit_linear <- c()
    
    # For each cemetery...
    for (i in 1:length( sort(unique(obs$cemetery)) ) ) {
    
      # Identify cemetery
      j <- sort(unique(obs$cemetery))[i]
      
      # Prepare data
      x0 <- subset(obs, cemetery == j)
      x1 <- c("graves", "time_base", "date", "cemetery")
      x2 <- x0[complete.cases(x0[, x1]), x1]
      
      # Fit linear interpolation
      x1 <- approx(x2[, "time_base"], x2[, "graves"], method = "linear", 
        xout = seq(min(x2$time_base), max(x2$time_base), by = 1), ties = mean)
      x1 <- as.data.frame(x1)
      colnames(x1) <- c("time_base", "y")
      x1 <- merge(x1, x0[, c("period_covid", "time_base")], by = "time_base", all = TRUE)
        
        # plot and predict across dataset for the cemetery
        actual <- f_plot_spline(x1, x0)    
        
      # Calculate new graves every day
        actual <- actual[order(actual[, "date"]), ]
        actual[, "new_graves_actual"] <- c(0, diff(actual$predictions) )
      
      # Restrict to data availability period
        # to period of maximum data availability across all cemeteries
        actual <- subset(actual, date <= date_max)
      
        # to period of minimum data availability in this cemetery
        date_start_local <- min(x2[, "date"])
        actual <- subset(actual, date >= date_start_local)
        
        # store output
        out_fit_linear <- rbind(out_fit_linear, actual)
        
      # Calculate burial rates and tolls in different periods, by epidemic start date
      for (k in 1:length(date_knot_options) ) {
        # identify date of epidemic start
        date_knot_k <- date_knot_options[k]
        
        # reclassify  period
        actual[, "period_covid"] <- ifelse(actual$date < date_knot_k, "baseline", "epidemic")
        actual[, "period_covid"] <- as.factor(actual$period_covid)
      
        # compute cemetery statistics
        x3 <- as.integer(i + length(unique(obs$cemetery)) * (k - 1) )
        out[x3, "days_baseline"] <- as.integer(date_knot_k - date_start_local)
        out[x3, "days_epidemic"] <- as.integer(date_max - date_knot_k)
        out[x3, "baseline_toll"] <- sum(subset(actual, period_covid == "baseline")$new_graves_actual, na.rm = TRUE)
        out[x3, "baseline_toll_complete"] <- sum(subset(actual, period_covid == "baseline" & date > date_min)$new_graves_actual, na.rm = TRUE)
        out[x3, "baseline_rate"] <- out[x3, "baseline_toll"] / out[x3, "days_baseline"]
        out[x3, "actual_toll"] <- sum(subset(actual, period_covid == "epidemic")$new_graves_actual, na.rm = TRUE)
        out[x3, "actual_rate"] <- out[x3, "actual_toll"] / out[x3, "days_epidemic"]
        out[x3, "excess_rate"] <- out[x3, "actual_rate"] - out[x3, "baseline_rate"] *
          mean(subset(actual, period_covid == "epidemic")[, "increase_high"], na.rm = TRUE) /
          mean(subset(actual, period_covid == "baseline")[, "increase_high"], na.rm = TRUE)
        out[x3, "counterfactual_toll"] <- out[x3, "baseline_rate"] * out[x3, "days_epidemic"] *
          mean(subset(actual, period_covid == "epidemic")[, "increase_high"], na.rm = TRUE) /
          mean(subset(actual, period_covid == "baseline")[, "increase_high"], na.rm = TRUE)
          # rough adjustment for increasing population denominator over time
        out[x3, "excess_toll"] <- out[x3, "actual_toll"] - out[x3, "counterfactual_toll"]
      }
    }    
    
    # Format output
    out[, grep("toll", colnames(out))] <- round(out[, grep("toll", colnames(out))], digits = 0)
    out[, grep("rate", colnames(out))] <- round(out[, grep("rate", colnames(out))], digits = 2)
    
    # Save output
    write.csv(out, "out_case-based_linear.csv", row.names = FALSE)
    
    # Compute overall mortality statistics for each alternative epidemic start date
      # set up output
      out_linear <- data.frame(date_knot_options)
      colnames(out_linear) <- "epidemic_start"
      out_linear[, c("baseline_death_rate", "excess_death_rate", "SMR", "excess_toll")] <- NA
    
    for (i in 1:length(date_knot_options) ) {
      # select output for this epidemic start date
      x1 <- subset(out, date_knot == date_knot_options[i])
      # baseline death (burial) rate per 1000 person-years (only computed for period of 100% data completeness)     
      out_linear[i, "baseline_death_rate"] <- sum(x1$baseline_toll_complete, na.rm = TRUE) * 1000 * 365 / 
        (mean(subset(obs, date %in% c(date_min : date_knot_options[i]))[, "pop_high"], na.rm = TRUE) * 
         as.integer(date_knot_options[i] - date_min ) )
      # excess death toll
      out_linear[i, "excess_toll"] <- sum(x1$excess_toll, na.rm = TRUE)
      # excess death (burial) rate per 1000 person-years
      out_linear[i, "excess_death_rate"] <- out_linear[i, "excess_toll"] * 1000 * 365 /
        (mean(subset(obs, date %in% c(date_knot_options[i] : date_max))[, "pop_high"], na.rm = TRUE) *  
         as.integer(date_max - date_knot_options[i] ) )
      # standardised mortality ratio (SMR)
      out_linear[i, "SMR"] <- sum(x1$actual_toll, na.rm = TRUE) / sum(x1$counterfactual_toll, na.rm = TRUE)
      }
 
      # Visualise and save results
      out_linear[, grep("toll", colnames(out_linear))] <- round(out_linear[, grep("toll", colnames(out_linear))], digits = 0)
      out_linear[, grep("rate", colnames(out_linear))] <- round(out_linear[, grep("rate", colnames(out_linear))], digits = 2)
      out_linear[, "SMR"] <- round(out_linear[, "SMR"], digits = 2)
      print(out_linear)
      write.csv(out_linear, paste("out_est_linear_", as.character(date_max), ".csv", sep = ""), row.names = FALSE)      
      
        # also save fit raw data
        write.csv(out_fit_linear, "out_fit_linear.csv", row.names = FALSE)

       
