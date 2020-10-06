#..........................................................................................
### +++++++ ESTIMATION OF COVID-19 ATTRIBUTABLE MORTALITY IN ADEN, YEMEN (2020) +++++++ ###
#..........................................................................................

#..........................................................................................
## ----------------- R CODE TO PREPARE DATA AND FIT STATISTICAL MODELS ----------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Sep 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................                            
### Preparing data for analysis
    
  #...................................
  ## Time series of cemeteries and days (as date and sequential day numbers)
    # Determine start and end of analysis period (based on extent of dataset)
    date_start <- as.Date( min(obs[, "date"], na.rm=TRUE) )
    date_end <- as.Date( max(obs[, "date"], na.rm=TRUE) )

    # Construct time series
    dates <- seq(from = date_start, to = date_end , by = 1)
    days <- data.frame(dates, c(0:(length(dates) - 1) ) )
    colnames(days) <- c("date", "time_base")
    ts <- expand.grid(cemeteries$cemetery, dates)
    colnames(ts) <- c("cemetery", "date")
    ts <- merge(ts, days, by="date")
    ts <- ts[order(ts[, "cemetery"], ts[, "date"]), ]
    ts[, "cemetery"] <- as.character(ts[, "cemetery"])

    # Create period variables
      # for added growth model
        # backward version - baseline time
        x1 <- rep(seq(-as.integer(date_end - date_start), 0, by = 1 ),
                  times = length(unique(obs$cemetery))
                 )
        ts[, "time_base_rev"] <- x1        
    
        # forward version - epidemic time
        x1 <- rep(c(rep(0, times = (date_knot - date_start) ),
                    seq(1, as.integer(date_end - date_knot + 1), by = 1 )
                    ), 
                  times = length(unique(obs$cemetery))
                 )
        ts[, "time_covid"] <- x1
        
        # backward version - epidemic time
        x1 <- rep(c(rep(-as.integer(date_end - date_knot), times = (date_knot - date_start) ),
                    seq(-as.integer(date_end - date_knot), 0, by = 1 )
                    ), 
                  times = length(unique(obs$cemetery))
                 )
        ts[, "time_covid_rev"] <- x1
        
        # knot version - baseline time
        x1 <- rep(c(seq(-as.integer(date_knot - date_start), 0, by = 1 ),
                    seq(1, as.integer(date_end - date_knot), by = 1)
                    ),
                  times = length(unique(obs$cemetery))
                 )
        ts[, "time_base_knot"] <- x1        
        
        # knot version - epidemic time
      
      ts[, "period_covid"] <- ifelse(ts$date < date_knot, "baseline", "epidemic")
      ts[, "period_covid"] <- as.factor(ts$period_covid)
      
      # for piecewise regression
      x1 <- rep(c(seq(as.integer(date_start - date_knot), 0, by = 1),
                  rep(0, times = as.integer(date_end - date_knot) )
                  ), 
                times = length(unique(obs$cemetery))
               )
      ts[, "time_piece1"] <- x1
      
      x1 <- rep(c(rep(0, times = as.integer(date_knot - date_start + 1)),
                  seq(0, as.integer(date_end - date_knot - 1), by = 1)
                  ),
                times = length(unique(obs$cemetery))
               )
      ts[, "time_piece2"] <- x1
      
  #...................................    
  ## Prepare datasets
    
    # Add 1 grave or 1 square metre to graves or area observations that equal 0 (so as to enable logging)
    obs[, "graves"] <- ifelse(obs$graves == 0, 1, obs$graves)  
    obs[, "area"] <- ifelse(obs$area == 0, 1, obs$area)  
      
    # Add secondary variables
      # starting number of graves (temporary - will be updated after imputation step below)
      x1 <- c()
      for (i in sort(unique(obs$cemetery)) ) {
        x1 <- c(x1, min(subset(obs, cemetery == i)$graves, na.rm = TRUE) )
      }
      x1 <- data.frame(sort(unique(obs$cemetery)), x1)
      colnames(x1) <- c("cemetery", "graves_start")
      obs <- merge(obs, x1, by = "cemetery", x.all = TRUE)

      # new graves and new area since the last observation
      obs <- obs[order(obs[, "cemetery"], obs[, "date"]), ]
      x1 <- c()
      x2 <- c()
      for (i in sort(unique(obs$cemetery)) ) {
        x3 <- subset(obs, cemetery == i)
        x1 <- c(x1, NA, diff (x3$graves ) )
        x2 <- c(x2, NA, diff (x3$area ) )

      }
      obs[, "new_graves"] <- x1
      obs[, "new_area"] <- x2
      

    # Merge dataset with time series
    obs <- merge(ts, obs, by = c("cemetery", "date"), all = TRUE)
    obs[, "date"] <- as.Date(obs$date)
    
    # Prepare population denominators and merge them in
      
      # smooth population across time series
      population[, "date"] <- ymd(paste(population$year, "-", population$month, "-15", sep = "") )
      population <- merge(population, days, by = "date", all.x = TRUE)
      population[, "time_base"] <- as.integer(population$date - date_start)
      x1 <- predict(smooth.spline(population[, "time_base"], y = population[, "pop_low"], spar = 0.2), 
       seq(min(days$time_base, na.rm = TRUE), max(days$time_base, na.rm = TRUE), by = 1) )
      x2 <- predict(smooth.spline(population[, "time_base"], y = population[, "pop_high"], spar = 0.2), 
       seq(min(days$time_base, na.rm = TRUE), max(days$time_base, na.rm = TRUE), by = 1) )
      x1 <- data.frame(x1$x, round(x1$y, digits = 0), round(x2$y, digits = 0 ))
      colnames(x1) <- c("time_base", "pop_low", "pop_high")
      
      # plot population trends
      x2 <- melt(x1, id = "time_base")
      colnames(x2) <- c("time_base", "series", "population")
      x2[, "estimate"] <- ifelse(x2[, "series"] == "pop_low", "low", "high")
      x2 <- merge(x2, days, by = "time_base", all = TRUE)
      plot <- ggplot(x2) + 
        geom_line(aes(x = date, y = population, colour = estimate, linetype = estimate), size = 1.5) + 
        scale_colour_manual(values = brewer_pal(palette = "BuGn")(9)[c(5,8)]) +
        scale_linetype_manual(values = c("solid", "twodash")) +
        scale_x_date("", minor_breaks=NULL, date_breaks="4 months", date_labels = "%b-%Y" ) +
        scale_y_continuous(labels = comma) +
        theme_bw() +
        labs (colour = "Estimate: ") +
        guides(linetype = FALSE) +
        theme(legend.position="top", legend.direction="horizontal") +
        theme(legend.title = element_text(color="grey20", size=11),
              legend.text = element_text(color="grey20", size=11),
              axis.title.x = element_text(color="grey20", size=11), 
              axis.text.x = element_text(color = "grey20", size=11, angle=315, hjust=0, vjust=0),               
              axis.line.y = element_line(color = "grey20"),
              axis.ticks.y = element_line(color = "grey20"),
              axis.text.y = element_text(color = "grey20", size=11),
              axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
              plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
              )
        
      plot  
      ggsave("population_over_time.png", width = 18, height = 10, units = "cm", dpi = "print") 
      
      # calculate annual increases, as a ratio from first value
      x1 <- merge(x1, days, by = "time_base")
      x1[, "increase_low"] <- x1[, "pop_low"] / x1[which.min(x1$date), "pop_low"]
      x1[, "increase_high"] <- x1[, "pop_high"] / x1[which.min(x1$date), "pop_high"]      

      # merge with observations
      obs <- merge(obs, x1[, c("date", "pop_low", "pop_high", "increase_low", "increase_high")], 
        by = "date", all.x = TRUE)
    
    # Merge cemetery meta-data with main dataset
    obs <- merge(obs, cemeteries[, c("cemetery", "cemetery_id", "district", "urban")], by = "cemetery", all.x = TRUE)
      # convert cemetery to factor (needed for random effect)
      obs[, "cemetery"] <- as.factor(obs[, "cemetery"])
      obs[, "cemetery_id"] <- as.factor(obs[, "cemetery_id"])
    
      # factorise variables as needed
      for (i in colnames(obs)) {
        if (class(obs[, i]) == "character" ) { obs[, i] <- as.factor(obs[, i])  }
      }
    
