#..........................................................................................
### +++++++ ESTIMATION OF COVID-19 ATTRIBUTABLE MORTALITY IN ADEN, YEMEN (2020) +++++++ ###
#..........................................................................................

#..........................................................................................
## ----------------- R CODE TO PREPARE DATA AND FIT STATISTICAL MODELS ----------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Sep 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Preparatory steps

  #...................................      
  ## Install or load required R packages
    
    # List of required packages
    x1 <- c("ggplot2", "scales", "readxl", "data.table", "lme4", "nlme", "broom.mixed", "lubridate", 
            "RColorBrewer", "lattice", "zoo", "car", "influence.ME", "MASS", "mgcv", "glmmTMB", "gamlss")
    
    # Install any packages not yet installed
    x2 <- x1 %in% row.names(installed.packages())
    if (any(x2 == FALSE)) { install.packages(x1[! x2]) }
    
    # Load all packages    
    lapply(x1, library, character.only = TRUE)
    

  #...................................      
  ## Starting steps

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    current_path = rstudioapi::getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print( getwd() )
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    

#.........................................................................................
### Specify parameters
    
    # Time window for rolling means of insecurity events (in days)
    roll <- 28
        
    # Candidate dates at which baseline period ends and epidemic epidemic period begins
    date_knot_options <- as.Date(c("1Feb2020", "1Mar2020", "1Apr2020"), "%d%b%Y")
      
      # Main date for analysis
      date_knot <- date_knot_options[3]
    
    # Number of folds for cross-validation
    k_folds <- NA   # NA means leave-one-out cross-validation
    
    # Whether an offset should be applied to the number of graves when fitting models (offset = starting number of graves)
    graves_offset <- TRUE
    
    # Number of bootstrap replicates for confidence interval estimation
    n_boot <- 1000
  

#.........................................................................................
### Bespoke functions
    
source("aden_covid_bespoke_functions.R")
        
#.........................................................................................
### Reading in required files

  #...................................      
  ## Variable dictionary
  dict <- read_excel("aden_covid_data.xlsx", sheet = "dictionary")
    # remove tibble nonsense
    dict <- as.data.frame(dict)

  #...................................      
  ## Read in all the datasets needed
    # which datasets
    x1 <- c("obs", "cemeteries", "population", "acled", "civil_registry_monthly", "civil_registry_weekly")
    
    # for each dataset...
    for (i in x1) {
      # read in
      assign(i, read_excel("aden_covid_data.xlsx", sheet = i) )
        # remove tibble nonsense
        assign(i, as.data.frame(get(i)) )
      # only keep needed columns
      x2 <- subset(dict, sheet == i)[, "use"]
      x2 <- which(! x2 %in% c("no") )
      x3 <- get(i)[, x2]
      assign(i, x3)
    }
    

#.........................................................................................                            
### Preparing data for analysis
    
source("aden_covid_prepare_data.R")
        
    
#.........................................................................................      
### Imputing missing burial count data
   
source("aden_covid_impute_graves.R")                         
    
    
#.........................................................................................      
### Describing trends in graves and other data characteristics
  
  #...................................   
  ## Plot trends in graves over time, by period and cemetery
    # Preparatory steps
      # data needed
      x1 <- obs[! is.na(obs$graves), c("date", "cemetery", "graves", "area", "period_covid")]

    # Draw plot
      plot <- ggplot(x1, aes(x = date, y = graves) ) +
              geom_line(linetype = "dashed", size = 0.7,  colour = brewer_pal(palette = "Dark2")(2)[2] ) +
              geom_point(aes(colour = period_covid), shape = 15, size = 2) +
              geom_line(aes(colour = period_covid), size = 1 ) +
              scale_colour_manual(values = brewer_pal(palette = "Dark2")(2)) +
              scale_y_continuous("number of graves") +
              theme_bw() +
              facet_wrap(~cemetery, nrow=5, scales = "free_y") +
              guides(fill = FALSE) +
              theme(legend.position="bottom", legend.direction="horizontal") +
              scale_x_date("", minor_breaks=NULL, date_breaks="4 months", date_labels = "%b-%Y" ) +
              labs(colour = "Period:  ") +
              theme(legend.title = element_text(color="grey20", size=11),
                    strip.text.x = element_text(color="grey20", size=11),
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
      ggsave("burials_over_time.png", width = 18, height = 18, units = "cm", dpi = "print")    

   
  #...................................   
  ## Plot trends in burial rate over time, by cemetery
    # Preparatory steps
      # data needed
      x1 <- obs[is.na(obs$graves) == FALSE, c("date", "cemetery", "graves")]
      
      # generate burial rate since the previous observation
      x1 <- x1[order(x1[, "cemetery"], x1[, "date"]), ]
      x2 <- c()
      for (i in sort(unique(x1$cemetery) ) ) {
        x3 <- subset(x1, cemetery == i)
        x2 <- c(x2, NA, diff(x3$graves) / as.integer(diff(x3$date)) )        

      }

      x1[, "burial_rate"] <- x2
      
      # lag burial rate so as to prepare data for step graph
      x2 <- c()
      for (i in sort(unique(x1$cemetery) ) ) {
        x4 <- subset(x1, cemetery == i)
        x2 <- c(x2, x4[-1, "burial_rate"], NA)  
      }

      x1[, "burial_rate_lag"] <- x2
      x1[, "burial_rate_lag"] <- ifelse(is.na(x1[, "burial_rate_lag"]), x1[, "burial_rate"], x1[, "burial_rate_lag"])
  
      
    # Draw plot
      plot <- ggplot(x1, aes(x = date, y = burial_rate_lag) ) +
              geom_step(size = 1, colour = brewer_pal(palette = "Dark2")(2)[1] ) +
              annotate(geom = "rect", xmin = date_knot, xmax = max(x1$date), ymin = 0, ymax = Inf,
                fill = brewer_pal(palette = "Reds")(9)[6], alpha = 0.3) +
              scale_y_continuous("mean new graves per day", limits = c(0, NA) ) +
              theme_bw() +
              facet_wrap(~cemetery, ncol=2, scales = "free_y") +
              scale_x_date("", minor_breaks=NULL, date_breaks="4 months", date_labels = "%b-%Y" ) +
              theme(strip.text.x = element_text(color="grey20", size=11),
                    axis.title.x = element_text(color="grey20", size=11), 
                    axis.text.x = element_text(color = "grey20", size=10, angle=315, hjust=0, vjust=0),               
                    axis.line.y = element_line(color = "grey20"),
                    axis.ticks.y = element_line(color = "grey20"),
                    axis.text.y = element_text(color = "grey20", size=11),
                    axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
                    plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
                    )
      plot
 
      ggsave("burial_rate_over_time_wide.png", width = 26, height = 18, units = "cm", dpi = "print")    
      ggsave("burial_rate_over_time_long.png", width = 20, height = 20, units = "cm", dpi = "print")    
      
  
  #...................................   
  ## Describe data for report
    # Create table
      # each cemetery
      out_descr <- data.frame(unique(obs$cemetery))
      colnames(out_descr) <- "cemetery" 
      
      # statistics for each cemetery...
      out_descr[, c("sub_district", "n_obs", "start_date", "end_date", "graves_start", "graves_end", "area_start", "area_end")] <- NA
      for (i in 1:length(out_descr$cemetery) ) {
        # select data
        x1 <- subset(obs, is.na(graves) == FALSE & cemetery == out_descr[i, "cemetery"] )
        # sub_district
        out_descr[i, "sub_district"] <- unique(as.character(x1$sub_district) )
        # number of observations per cemetery
        out_descr[i, "n_obs"] <- nrow(x1)
        # start date
        out_descr[i, "start_date"] <- min(x1$date, na.rm = TRUE)
        # end date
        out_descr[i, "end_date"] <- max(x1$date, na.rm = TRUE)
        # starting number of graves
        out_descr[i, "graves_start"] <- x1[which.min(x1$date), "graves"] 
        # ending number of graves
        out_descr[i, "graves_end"] <- x1[which.max(x1$date), "graves"] 
        # starting surface area
        out_descr[i, "area_start"] <- x1[which.min(x1$date), "area"] 
        # ending surface area
        out_descr[i, "area_end"] <- x1[which.max(x1$date), "area"]             
      }
    # Save
      out_descr[, "start_date"] <- as.Date(out_descr[, "start_date"])
      out_descr[, "end_date"] <- as.Date(out_descr[, "end_date"])
      out_descr[, "sub_district"] <- as.factor(out_descr[, "sub_district"])
      write.csv(out_descr, "out_table_description_cemeteries.csv", row.names = FALSE)
    

#.........................................................................................                            
### Implementing approach 1: Case-based method (analysing each cemetery alone)

source("aden_covid_case_based.R")
      
      
#.........................................................................................                            
### Implementing approach 2: Multi-level model (analysing all cemeteries together)

source("aden_covid_gamlss_model.R")
           
                
#.........................................................................................                            
### Comparing estimates with Aden Civil Registry (CR) data (for brevity - only for model without conflict intensity)

  #...................................   
  ## Comparison by month
      
    # Establish boundaries of monthly 'baseline' based on CR data    
      x4 <- subset(civil_registry_monthly, year != 2020)    
      cr_base <- data.frame(unique(x4$month), NA, NA, NA )
      colnames(cr_base) <- c("month", "baseline_mid", "baseline_low", "baseline_high")
      for (i in unique(x4$month)) {cr_base[cr_base$month == i, 2:4] <- c(median(subset(x4, month == i)$deaths), range(subset(x4, month == i)$deaths) )}
      cr_base_mid <- median(cr_base$baseline_mid)
      cr_base_low <- median(cr_base$baseline_low)
      cr_base_high <- median(cr_base$baseline_high)
      
    # Plot monthly patterns in total deaths (GAMLSS-predicted vs. approach 1 vs. CR)
      # aggregate approach 1/spline data by month
        x3 <- unique(ceiling_date(out_fit_spline$date, "month") ) - 1
        x2 <- subset(out_fit_spline, date %in% seq(max(out_descr$start_date), min(out_descr$end_date), by = 1 ) )        
        x2 <- subset(x2, date %in% x3)
        out_month_spline <- aggregate(x2[, "predictions"], by = list(date = x2[, "date"]), FUN = sum)
        out_month_spline[, "new_graves_spline"] <-round( c(NA, diff(out_month_spline[, 2])), digits = 0)
        out_month_spline[, "month"] <- month(out_month_spline$date)
        out_month_spline[, "year"] <- year(out_month_spline$date)

      # aggregate approach 1/linear data by month
        x3 <- unique(ceiling_date(out_fit_linear$date, "month") ) - 1
        x2 <- subset(out_fit_linear, date %in% seq(max(out_descr$start_date), min(out_descr$end_date), by = 1 ) )        
        x2 <- subset(x2, date %in% x3)
        out_month_linear <- aggregate(x2[, "predictions"], by = list(date = x2[, "date"]), FUN = sum)
        out_month_linear[, "new_graves_linear"] <-round( c(NA, diff(out_month_linear[, 2])), digits = 0)
        out_month_linear[, "month"] <- month(out_month_linear$date)
        out_month_linear[, "year"] <- year(out_month_linear$date)
        
      # prepare data
        x1 <- merge(out_month, civil_registry_monthly, by = c("year", "month"),  all.x = TRUE)
        x1 <- merge(x1, out_month_spline[, c("month", "year", "new_graves_spline")], by = c("month", "year") , all.x = TRUE)        
        x1 <- merge(x1, out_month_linear[, c("month", "year", "new_graves_linear")], by = c("month", "year") , all.x = TRUE)        
        
        x1 <- x1[, c("date", "new_actual_est", "deaths", "new_graves_spline", "new_graves_linear")]
        x2 <- x1[, c("date", "new_actual_est")]; colnames(x2) <- c("date", "deaths")
        x3 <- x1[, c("date", "deaths")]; colnames(x3) <- c("date", "deaths")
        x4 <- x1[, c("date", "new_graves_spline")]; colnames(x4) <- c("date", "deaths")
        x5 <- x1[, c("date", "new_graves_linear")]; colnames(x5) <- c("date", "deaths")
        
        x1 <- rbind(x2, x3, x4, x5)
        x1[, "estimate"] <- c(rep("GAMLSS", nrow(x2)), rep("Civil Registry", nrow(x3)), 
                              rep("spline smooth", nrow(x4)), rep("linear interpolation", nrow(x5)) )
        
        x2 <- subset(x1, estimate != "Civil Registry")
        
        x3 <- c(subset(x1, estimate == "Civil Registry")$deaths, rep(NA, nrow(x2)*2/3 ) )
        x2[, "cr_deaths"] <- x3
        
        x2[, "cr_base_mid"] <- cr_base_mid
        x2[, "cr_base_low"] <- cr_base_low
        x2[, "cr_base_high"] <- cr_base_high
        
        # adjust dates so that month is centered correctly
        x2[, "date"] <- x2[, "date"] - days_in_month(month(x2[, "date"])) + 1
        
      # plot
        plot <- ggplot(x2 ) +
           geom_line(aes(x = date, y = deaths, colour = estimate, linetype = estimate), size = 0.8) +
           geom_point(aes(x = date, y = deaths, colour = estimate, shape = estimate), size = 2) +
           scale_colour_manual(values = brewer_pal(palette = "Set1")(9)[c(1,4,5)] ) +
           scale_linetype_manual(values = c("solid", "dashed", "twodash")) +
           scale_shape_manual(values = c(16, 0, 2)) +
           geom_col(aes(x = date, y = cr_deaths) ,
             colour = brewer_pal(palette = "Set1")(3)[2], 
             fill = brewer_pal(palette = "Set1")(3)[2], alpha = 0.3) +
           geom_line(aes(x = date, y = cr_base_mid), linetype = "dotted", size = 0.7) +
           geom_ribbon(aes(x = date, ymin = cr_base_low, ymax = cr_base_high), fill = "grey", alpha = 0.2) +
           scale_y_continuous("new burials" , breaks=seq(0, 2000, by = 200)) +
           scale_x_date("", minor_breaks=NULL, date_breaks="4 months", date_labels = "%b-%Y" ) +
           theme_bw() +
           guides(fill = FALSE) +
           theme(legend.position="bottom", legend.direction="horizontal") +
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
        ggsave("comparison_civil_registry_monthly.png", width = 25, height = 15, units = "cm", dpi = "print")    

              
    # CR vs. GAMLSS actual and excess deaths / death rates: 1 Apr to 31 Jul
      # set up output
      out <- data.frame(matrix(NA, ncol = 2, nrow = 4))
      colnames(out)<- c("GAMLSS", "Civil_registry")
      rownames(out) <- c("actual_est", "excess_est", "actual_est_rate", "excess_est_rate")
      date_cr <- as.Date("31Jul2020", "%d%b%Y")
        
      # GAMLSS
      out[c("actual_est", "excess_est"), "GAMLSS"] <- colSums(subset(est, date == date_cr)[, c("actual_est", "excess_est")] , na.rm = TRUE) - 
        colSums(subset(est, date == date_knot)[, c("actual_est", "excess_est")] , na.rm = TRUE)
      out[c("actual_est_rate", "excess_est_rate"), "GAMLSS"] <- round(out[c("actual_est", "excess_est"), "GAMLSS"] * 1000 * 365 / 
        ( mean(obs$pop_high) * as.integer(date_cr - date_knot) ), digits = 2)

      # Civil registry
      out["actual_est", "Civil_registry"] <- sum(subset(civil_registry_monthly, year == 2020 & month %in% c(4, 5, 6, 7))[, "deaths"])
      out["excess_est", "Civil_registry"] <- out["actual_est", "Civil_registry"] - cr_base_mid * 4
      out[c("actual_est_rate", "excess_est_rate"), "Civil_registry"] <- round(out[c("actual_est", "excess_est"), "Civil_registry"] * 1000 * 365 / 
        ( mean(obs$pop_high) * as.integer(date_cr - date_knot) ), digits = 2)
      
      write.csv(out, "out_est_gamlss_vs_civil_2020-07-31.csv")
      
  #...................................   
  ## Comparison by week (2020 only)
    
    # Prepare data
      # CR data
      colnames(civil_registry_weekly) <- c("year", "week", "date", "deaths")
      
      # aggregate approach 1/spline data by week
        x3 <- unique(ceiling_date(out_fit_spline$date, "week") ) - 1
        x2 <- subset(out_fit_spline, date %in% seq(max(out_descr$start_date), min(out_descr$end_date), by = 1 ) )        
        x2 <- subset(x2, date %in% x3)
        out_week_spline <- aggregate(x2[, "predictions"], by = list(date = x2[, "date"]), FUN = sum)
        out_week_spline[, "new_graves_spline"] <-round( c(NA, diff(out_week_spline[, 2])), digits = 0)
        out_week_spline[, "week"] <- isoweek(out_week_spline$date)
        out_week_spline[, "year"] <- year(out_week_spline$date)

      # aggregate approach 1/linear data by week
        x3 <- unique(ceiling_date(out_fit_linear$date, "week") ) - 1
        x2 <- subset(out_fit_linear, date %in% seq(max(out_descr$start_date), min(out_descr$end_date), by = 1 ) )        
        x2 <- subset(x2, date %in% x3)
        out_week_linear <- aggregate(x2[, "predictions"], by = list(date = x2[, "date"]), FUN = sum)
        out_week_linear[, "new_graves_linear"] <-round( c(NA, diff(out_week_linear[, 2])), digits = 0)
        out_week_linear[, "week"] <- isoweek(out_week_linear$date)
        out_week_linear[, "year"] <- year(out_week_linear$date)
      
      # merge    
      x1 <- merge(out_week, civil_registry_weekly[, c("year", "week", "deaths")], by = c("year", "week"), all.x = TRUE)    
      x1 <- merge(x1, out_week_spline[, c("date", "new_graves_spline")], by = "date", all.x = TRUE)    
      x1 <- merge(x1, out_week_linear[, c("date", "new_graves_linear")], by = "date", all.x = TRUE)    
       
      x1 <- x1[, c("date", "new_actual_est", "deaths", "new_graves_spline", "new_graves_linear")]
      x2 <- x1[, c("date", "new_actual_est")]; colnames(x2) <- c("date", "deaths")
      x3 <- x1[, c("date", "deaths")]; colnames(x3) <- c("date", "deaths")
      x4 <- x1[, c("date", "new_graves_spline")]; colnames(x4) <- c("date", "deaths")
      x5 <- x1[, c("date", "new_graves_linear")]; colnames(x5) <- c("date", "deaths")
        
      x1 <- rbind(x2, x3, x4, x5)
      x1[, "estimate"] <- c(rep("GAMLSS", nrow(x2)), rep("Civil Registry", nrow(x3)), 
                            rep("spline smooth", nrow(x4)), rep("linear interpolation", nrow(x5)) )
        
      x2 <- subset(x1, estimate != "Civil Registry")
        
      x3 <- c(subset(x1, estimate == "Civil Registry")$deaths, rep(NA, nrow(x2)*2/3 ) )
      x2[, "cr_deaths"] <- x3
      x2 <- subset(x2, date > "2019-12-31")
      
      # add average civil registry weekly deaths from 2016-2019
      x2[, "cr_base_mid"] <- cr_base_mid * 7 / 30.41
      x2[, "cr_base_low"] <- cr_base_low * 7 / 30.41
      x2[, "cr_base_high"] <- cr_base_high * 7 / 30.41
       
    # Plot    
        plot <- ggplot(x2 ) +
           geom_line(aes(x = date, y = deaths, colour = estimate, linetype = estimate), size = 0.8) +
           geom_point(aes(x = date, y = deaths, colour = estimate, shape = estimate), size = 2) +
           geom_line(aes(x = date, y = cr_base_mid), linetype = "dotted", size = 0.7) +
           geom_ribbon(aes(x = date, ymin = cr_base_low, ymax = cr_base_high), fill = "grey", alpha = 0.2) +
           scale_colour_manual(values = brewer_pal(palette = "Set1")(9)[c(1,4,5)] ) +
           scale_linetype_manual(values = c("solid", "dashed", "twodash")) +
           scale_shape_manual(values = c(16, 0, 2)) +
           geom_col(aes(x = date, y = cr_deaths) ,
             colour = brewer_pal(palette = "Set1")(3)[2], 
             fill = brewer_pal(palette = "Set1")(3)[2], alpha = 0.3) +
           scale_y_continuous("new burials" , breaks=seq(0, 600, by = 100)) +
           scale_x_date("", minor_breaks=NULL, date_breaks="2 weeks" ) +
           theme_bw() +
           guides(fill = FALSE) +
           theme(legend.position="bottom", legend.direction="horizontal") +
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
        ggsave("comparison_civil_registry_weekly.png", width = 25, height = 15, units = "cm", dpi = "print")    
        

          
#.........................................................................................
### ENDS



  