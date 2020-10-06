#..........................................................................................
### +++++++ ESTIMATION OF COVID-19 ATTRIBUTABLE MORTALITY IN ADEN, YEMEN (2020) +++++++ ###
#..........................................................................................

#..........................................................................................
## ----------------- R CODE TO PREPARE DATA AND FIT STATISTICAL MODELS ----------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Sep 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................                            
### Implementing approach 2: Multi-level model (analysing all cemeteries together)

  #...................................   
  ## Observe distribution of graves (dependent variable) and apply any transformation / offset
    # linear scale
      f_hist("graves", obs, c(NA, NA) )
      # looks overdispersed
    # log scale
      obs[, "graves_ln"] <- log(obs[, "graves"])
      f_hist("graves_ln", obs, c(NA, NA) )
    # square root scale
      obs[, "graves_sqrt"] <- sqrt(obs[, "graves"])
      f_hist("graves_sqrt", obs, c(NA, NA) )
      
    # transform graves by offsetting all cemeteries by the starting number of graves (yields far better fits)
      if (graves_offset == TRUE) {
        obs[, "graves"] <- obs[, "graves"] - obs[, "graves_start"] + 1 # +1 to avoid infinite natural logs
      }
      
  #...................................   
  ## Fit a discrete count generalised additive mixed model (GAMM), using GAMLSS package
    
    # Generate data
    x1 <- c("graves", "time_base", "cemetery_id", "time_covid", "time_base_rev", "time_covid_rev",
            "time_base_knot", "time_piece1", "time_piece2")
    obs_fit <- obs[complete.cases(obs[, x1]), x1]
  
    # Help to select the right model family and fitting method
    lms(graves, time_base, data = obs_fit, families = c("PO", "NBI", "NBII"))
      # suggests best to use NBI family and penalised B-splines (pb)
    
    # Added growth model
      fit <- gamlss(graves ~ pb(time_base_rev, df = 2) + pb(time_covid_rev, df = 2)
        + re(random = ~1 + time_base_rev | cemetery_id), 
        sigma.formula=~pb(time_base_rev) + pb(time_covid_rev), data = obs_fit, family = NBI)
      summary(fit)
      plot(fit)
      x1 <- f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
      write.csv(summary(fit), "out_gamlss_model.csv", row.names = FALSE)
####TO DO: FIX LOOCV      
      
      # plot fit
        # prepare data
        x1[, "scenario"] <- ifelse(x1$period_covid == "epidemic", "actual", "counterfactual")
      
        # plot  
        plot <- ggplot(x1) +
           geom_point(aes(x = date, y = graves, colour = scenario), size = 1.5) +
           geom_line(aes(x = date, y = predictions, colour = scenario), size = 0.9 ) +
           geom_line(aes(x = date, y = counterfactuals), linetype = "dashed",
             colour = brewer_pal(palette = "Dark2")(2)[1], size = 0.7 ) +
           scale_colour_manual(values = brewer_pal(palette = "Dark2")(2)[c(2,1)]) +
           scale_y_continuous("cumulative number of graves") +
           theme_bw() +
           facet_wrap(~cemetery, ncol = 3, scales = "free_y") +
           guides(fill = FALSE) +
           theme(legend.position="bottom", legend.direction="horizontal") +
           scale_x_date("", minor_breaks=NULL, date_breaks="4 months", date_labels = "%b-%Y" ) +
           labs(colour = "Scenario:  ") +
           theme(legend.title = element_text(color="grey20", size=11),
                 strip.text.x = element_text(color="grey20", size=11),
                 legend.text = element_text(color="grey20", size=11),
                 axis.title.x = element_text(color="grey20", size=11), 
                 axis.text.x = element_text(color = "grey20", size=9, angle=315, hjust=0, vjust=0),               
                 axis.line.y = element_line(color = "grey20"),
                 axis.ticks.y = element_line(color = "grey20"),
                 axis.text.y = element_text(color = "grey20", size=11),
                 axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
                 plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
                 )
      plot
      
      ggsave("gamlss_model_fit_wide.png", width = 30, height = 18, units = "cm", dpi = "print")    
      ggsave("gamlss_model_fit_long.png", width = 25, height = 25, units = "cm", dpi = "print")  
      
      # now zoom in on desired region of plot
      x2 <- seq(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)), by = 1)
      plot <- plot %+% subset(x1, date %in% x2 ) + scale_x_date("", date_breaks="1 month", date_labels = "%b-%Y" )
      plot
      
      ggsave("gamlss_model_fit__2020_wide.png", width = 30, height = 18, units = "cm", dpi = "print")    
      ggsave("gamlss_model_fit_2020_long.png", width = 25, height = 25, units = "cm", dpi = "print")  
     
  #...................................   
  ## Excess mortality estimation
    
    # Prepare counterfactual dataset
      x4 <- grep("time_covid|time_piece2", all.vars(formula(fit)), value = TRUE)
      x5 <- min(subset(obs, period_covid == "epidemic")[, "date"], na.rm = TRUE)
      x5 <- obs[obs$date == x5, x4]
      x5 <- unique(x5)
      counter <- obs
      counter[, x4] <- x5
        
    # Predict actual and counterfactual scenarios  
      # point estimate and 95%CIs
      x3 <- f_interval(fit, n_boot, obs, FALSE)
      x4 <- f_interval(fit, n_boot, counter, FALSE)
        
      # for bootstrapped CI of excess death toll
      x5 <- f_interval(fit, n_boot, obs, TRUE)
      x6 <- f_interval(fit, n_boot, counter, TRUE)
        
      
    # Plot actual and counterfactual trends, for all cemeteries together
      # create results database
      est <- merge(x3, x4[, c("cemetery", "date", "predicted", "predicted_lci", "predicted_uci")],
                by = c("cemetery", "date"))
      colnames(est) <- c( colnames(obs), 
                          "actual_est", "actual_lci", "actual_uci",
                          "counter_est", "counter_lci", "counter_uci"
                        )
      # prepare data for plotting
        # aggregate all cemeteries
        x1 <- aggregate(est[, c("actual_est", "actual_lci", "actual_uci", "counter_est", "counter_lci", "counter_uci")],
                by = list(est$date), FUN = sum)
        colnames(x1)[1] <- "date"
        # select only 2020
        x1 <- subset(x1, date >= as.Date("2020-01-01"))
        # reshape
        x2 <- x1[, c("date", "actual_est", "actual_lci", "actual_uci")]
        x3 <- x1[, c("date", "counter_est", "counter_lci", "counter_uci")]
        colnames(x2) <- c("date", "point", "lci", "uci")
        colnames(x3) <- c("date", "point", "lci", "uci")
        
        x1 <- as.data.frame(rbind(x2, x3) )
        x1[, "scenario"] <- c(rep("actual", nrow(x2)), rep("counterfactual", nrow(x3)) ) 
                
      # plot
        plot <- ggplot(x1) +
           geom_line(aes(x = date, y = point, colour = scenario), size = 1) +
           geom_ribbon(aes(x = date, ymin = lci, ymax = uci, fill = scenario), alpha = 0.2) +
           scale_y_continuous("estimated number of graves" , labels = comma, 
             breaks = seq(10000, 17000, 1000)) +
           scale_x_date("", minor_breaks=NULL, date_breaks="1 month", date_labels = "%b-%Y" ) +
           scale_colour_manual(values = cbPalette[c(7,4)]) +
           scale_fill_manual(values = cbPalette[c(7,4)]) +
           theme_bw() +
           guides(fill = FALSE) +
           theme(legend.position="bottom", legend.direction="horizontal") +
           labs(colour = "Scenario:  ") +
           theme(legend.title = element_text(color="grey20", size=11),
                 legend.text = element_text(color="grey20", size=11),
                 axis.title.x = element_text(color="grey20", size=11), 
                 axis.text.x = element_text(color = "grey20", size=10, angle=315, hjust=0, vjust=0),               
                 axis.line.y = element_line(color = "grey20"),
                 axis.ticks.y = element_line(color = "grey20"),
                 axis.text.y = element_text(color = "grey20", size=11),
                 axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
                 plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
                 )
      plot
      ggsave("gamlss_est_CI_2020_wide.png", width = 25, height = 15, units = "cm", dpi = "print")    
      
      
    # Compute excess mortality    
      # excess deaths from each bootstrap replicate
      excess_sim <- t(x5 - x6)
      
      # point estimate and confidence interval
      excess_est <- t(apply(excess_sim, 1, quantile, prob = c(0.500, 0.025, 0.975), na.rm = TRUE) )
    
      # merge with main database of estimates
      est <- cbind(est, excess_est)
      colnames(est) <- c(colnames(est)[1:(ncol(est) - 3)], "excess_est", "excess_lci", "excess_uci")
      est[, c("excess_est", "excess_lci", "excess_uci")] <- round(est[, c("excess_est", "excess_lci", "excess_uci")], digits = 0)
        
      # tabulate by cemetery, by date, by week, by month and overall
        x1 <- c("actual_est", "actual_lci", "actual_uci",
                "counter_est", "counter_lci", "counter_uci",
                "excess_est", "excess_lci", "excess_uci")
        x4 <- c("actual_est", "counter_est", "excess_est")
        x5 <- c("new_actual_est", "new_counter_est", "new_excess_est")
        
        # by cemetery
        x2 <- subset(est, date == date_end)
        out_cemetery <- aggregate(x2[, x1], by = list(cemetery = x2[, "cemetery"]), FUN = sum)
        out_cemetery[, paste(x1, "_rate", sep = "")] <- round(out_cemetery[, x1] / as.integer(date_end - date_knot), digits = 2)
        
        out_cemetery[, "actual"] <- paste(out_cemetery[, "actual_est"], " (", 
          out_cemetery[, "actual_lci"], " to ", out_cemetery[, "actual_uci"], ")", sep = "" )
        out_cemetery[, "counter"] <- paste(out_cemetery[, "counter_est"], " (", 
          out_cemetery[, "counter_lci"], " to ", out_cemetery[, "counter_uci"], ")", sep = "" )
        out_cemetery[, "excess"] <- paste(out_cemetery[, "excess_est"], " (", 
          out_cemetery[, "excess_lci"], " to ", out_cemetery[, "excess_uci"], ")", sep = "" )
        
        out_cemetery[, "actual_rate"] <- paste(out_cemetery[, "actual_est_rate"], " (", 
          out_cemetery[, "actual_lci_rate"], " to ", out_cemetery[, "actual_uci_rate"], ")", sep = "" )
        out_cemetery[, "counter_rate"] <- paste(out_cemetery[, "counter_est_rate"], " (", 
          out_cemetery[, "counter_lci_rate"], " to ", out_cemetery[, "counter_uci_rate"], ")", sep = "" )
        out_cemetery[, "excess_rate"] <- paste(out_cemetery[, "excess_est_rate"], " (", 
          out_cemetery[, "excess_lci_rate"], " to ", out_cemetery[, "excess_uci_rate"], ")", sep = "" )

        write.csv(out_cemetery[, c("cemetery", "actual", "counter", "excess", "actual_rate", "counter_rate", "excess_rate")], 
          "out_est_gamlss_by_cemetery.csv", row.names = FALSE)
        
        # by date
        out_date <- aggregate(est[, x1], by = list(date = est[, "date"]), FUN = sum)
        out_date[, x5] <- data.frame(rbind(matrix(NA, 1, 3), diff(as.matrix(out_date[, x4])) ))
        x6 <- aggregate(obs[, c("pop_low", "pop_high")], by = list (date = obs$date), FUN = unique)
        out_date <- merge(out_date, x6, by = "date", all.x = TRUE)
        out_date[, paste(x5, "_rate", sep ="")] <- out_date[, x5] * 1000 * 365 / out_date$pop_high
        write.csv(out_date, "out_est_gamlss_by_date.csv", row.names = FALSE)
        
        # by week
        x3 <- unique(ceiling_date(est$date, "week") ) - 1
        x2 <- subset(est, date %in% x3)
        out_week <- aggregate(x2[, x1], by = list(date = x2[, "date"]), FUN = sum)
        out_week[, x5] <- data.frame(rbind(matrix(NA, 1, 3), diff(as.matrix(out_week[, x4])) ))
        out_week[, "year"] <- year(out_week$date)
        out_week[, "week"] <- isoweek(out_week$date)
        out_week <- merge(out_week, x6, by = "date", all.x = TRUE)
        out_week[, paste(x5, "_rate", sep ="")] <- out_week[, x5] * 1000 * 365 / (out_week$pop_high * 7)
        write.csv(out_week, "out_est_gamlss_by_week.csv", row.names = FALSE)
        
        # by month
        x3 <- unique(ceiling_date(est$date, "month") ) - 1
        x2 <- subset(est, date %in% x3)
        out_month <- aggregate(x2[, x1], by = list(date = x2[, "date"]), FUN = sum)
        out_month[, x5] <- data.frame(rbind(matrix(NA, 1, 3), diff(as.matrix(out_month[, x4])) ))
        out_month[, "month"] <- month(out_month$date)
        out_month[, "year"] <- year(out_month$date)
        out_month <- merge(out_month, x6, by = "date", all.x = TRUE)
        out_month[, paste(x5, "_rate", sep ="")] <- out_month[, x5] * 1000 * 365 / (out_month$pop_high * 30.41)
        write.csv(out_month, "out_est_gamlss_by_month.csv", row.names = FALSE)
        
        # overall (considering epidemic period)
          # capping to date when all cemeteries have ground data (as for Approach 1)
          out_date_max <- colSums(subset(est, date == date_max)[, x1] , na.rm = TRUE) - 
            colSums(subset(est, date == date_knot)[, x1] , na.rm = TRUE)
          out_date_max[paste(x1, "_rate", sep ="")] <- round(out_date_max[x1] * 1000 * 365 / 
            ( mean(obs$pop_high) * as.integer(date_max - date_knot) ), digits = 2)
          out_date_max["actual"] <- paste(out_date_max["actual_est"], " (", 
            out_date_max["actual_lci"], " to ", out_date_max["actual_uci"], ")", sep = "" )
          out_date_max["counter"] <- paste(out_date_max["counter_est"], " (", 
            out_date_max["counter_lci"], " to ", out_date_max["counter_uci"], ")", sep = "" )
          out_date_max["excess"] <- paste(out_date_max["excess_est"], " (", 
            out_date_max["excess_lci"], " to ", out_date_max["excess_uci"], ")", sep = "" )
          
          out_date_max["actual_rate"] <- paste(out_date_max["actual_est_rate"], " (", 
            out_date_max["actual_lci_rate"], " to ", out_date_max["actual_uci_rate"], ")", sep = "" )
          out_date_max["counter_rate"] <- paste(out_date_max["counter_est_rate"], " (", 
            out_date_max["counter_lci_rate"], " to ", out_date_max["counter_uci_rate"], ")", sep = "" )
          out_date_max["excess_rate"] <- paste(out_date_max["excess_est_rate"], " (", 
            out_date_max["excess_lci_rate"], " to ", out_date_max["excess_uci_rate"], ")", sep = "" )
          
          write.csv(out_date_max[c("actual", "counter", "excess", "actual_rate", "counter_rate", "excess_rate")], 
            "out_est_gamlss_by_date_max.csv", row.names = TRUE)
            
          # entire period
          out_date_end <- colSums(subset(est, date == date_end)[, x1] , na.rm = TRUE) - 
            colSums(subset(est, date == date_knot)[, x1] , na.rm = TRUE)
          out_date_end[paste(x1, "_rate", sep ="")] <- round(out_date_end[x1] * 1000 * 365 / 
            ( mean(obs$pop_high) * as.integer(date_end - date_knot) ), digits = 2)
          out_date_end["actual"] <- paste(out_date_end["actual_est"], " (", 
            out_date_end["actual_lci"], " to ", out_date_end["actual_uci"], ")", sep = "" )
          out_date_end["counter"] <- paste(out_date_end["counter_est"], " (", 
            out_date_end["counter_lci"], " to ", out_date_end["counter_uci"], ")", sep = "" )
          out_date_end["excess"] <- paste(out_date_end["excess_est"], " (", 
            out_date_end["excess_lci"], " to ", out_date_end["excess_uci"], ")", sep = "" )
          
          out_date_end["actual_rate"] <- paste(out_date_end["actual_est_rate"], " (", 
            out_date_end["actual_lci_rate"], " to ", out_date_end["actual_uci_rate"], ")", sep = "" )
          out_date_end["counter_rate"] <- paste(out_date_end["counter_est_rate"], " (", 
            out_date_end["counter_lci_rate"], " to ", out_date_end["counter_uci_rate"], ")", sep = "" )
          out_date_end["excess_rate"] <- paste(out_date_end["excess_est_rate"], " (", 
            out_date_end["excess_lci_rate"], " to ", out_date_end["excess_uci_rate"], ")", sep = "" )
          
          write.csv(out_date_end[c("actual", "counter", "excess", "actual_rate", "counter_rate", "excess_rate")], 
            "out_est_gamlss_by_date_end.csv", row.names = TRUE)
          