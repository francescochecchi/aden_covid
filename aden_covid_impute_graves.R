#..........................................................................................
### +++++++ ESTIMATION OF COVID-19 ATTRIBUTABLE MORTALITY IN ADEN, YEMEN (2020) +++++++ ###
#..........................................................................................

#..........................................................................................
## ----------------- R CODE TO PREPARE DATA AND FIT STATISTICAL MODELS ----------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Sep 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................      
### Imputing missing burial count data
    
  #...................................   
  ## Visualise correlation between graves and surface area, depending on in-filling
    # Graves vs. surface area
      plot <- ggplot(subset(obs, ! is.na(area)) ) +
              geom_point(mapping = aes(colour = infilling, x = area, y = graves), size = 2) +
              scale_colour_manual(values = brewer_pal(palette = "Dark2")(3)) +
              scale_y_continuous("number of graves", minor_breaks=NULL) +
              scale_x_continuous("surface area (square metres)", minor_breaks=NULL) +
              theme_bw() +
              theme(legend.position="top", legend.direction="horizontal") +
              labs(colour = "burial pattern:  ") +
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

    # New graves vs. new surface area
      plot <- ggplot(subset(obs, ! is.na(new_area)) ) +
              geom_point(mapping = aes(colour = infilling, x = new_area, y = new_graves), size = 2) +
              scale_colour_manual(values = brewer_pal(palette = "Dark2")(3)) +
              scale_y_continuous("number of new graves", minor_breaks=NULL, trans = "log2",
                breaks = c(0,50,100,250,500,1000,2000)) +
              scale_x_continuous("new surface area (square metres)", trans = "log2",
                breaks = c(0,50,100,250,500,1000,2000,5000,10000), labels = comma) +
              theme_bw() +
              theme(legend.position="top", legend.direction="horizontal") +
              labs(colour = "Burial pattern:  ") +
              theme(legend.title = element_text(color="grey20", size=11),
                    legend.text = element_text(color="grey20", size=11),
                    axis.title.x = element_text(color="grey20", size=11), 
                    axis.text.x = element_text(color = "grey20", size=11, vjust=0),               
                    axis.line.y = element_line(color = "grey20"),
                    axis.ticks.y = element_line(color = "grey20"),
                    axis.text.y = element_text(color = "grey20", size=11),
                    axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
                    plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
                    )
      plot
      ggsave("new_graves_vs_new_area.png", width = 18, height = 10, units = "cm", dpi = "print")    

                
 #...................................   
  ## Visualise distribution of graves and area

    # Graves and area    
      # linear scale
      f_hist("graves", obs, c(NA, NA) )
      f_hist("area", obs, c(NA, NA) ) 
      # log scale
      obs[, "graves_ln"] <- log(obs[, "graves"])
      f_hist("graves_ln", obs, c(NA, NA) ) 
      obs[, "area_ln"] <- log(obs[, "area"])
      f_hist("area_ln", obs, c(NA, NA) ) 

    # New graves and new area    
      # linear scale
      f_hist("new_graves", obs, c(NA, NA) )
      f_hist("new_area", obs, c(NA, NA) ) 
      # log scale
      obs[, "new_graves_ln"] <- log(obs[, "new_graves"])
      f_hist("new_graves_ln", obs, c(NA, NA) ) 
      obs[, "new_area_ln"] <- log(obs[, "new_area"])
      f_hist("new_area_ln", obs, c(NA, NA) ) 
      
            
  #...................................   
  ## Predictive model of graves as a function of surface area
    # Discrete generalised linear mixed model; log(starting n of graves) as offset

      # new graves (more robust than graves as it constrains total predicted graves to >= 0, as it must be)
      fit <- f_glmm("new_graves", c("new_area_ln"), "(1 | cemetery)", "graves_start", obs, "nbinom1") 
      x1 <- f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, TRUE)

    # Plot CV output
      plot <- ggplot(x1) +
          geom_point(aes(x = observations, y = predictions), size=2, colour = brewer_pal(palette = "Dark2")(2)[1]) + 
          theme_bw() +
          scale_x_continuous("observed", trans = "log2", breaks = c(0,25,50,100,250,500,1000,2000) ) +
          scale_y_continuous("predicted", trans = "log2", breaks = c(0,25,50,100,250,500,1000,2000)) +  
          geom_abline(intercept = 0, slope = 1, colour = brewer_pal(palette = "Dark2")(2)[2] ) +
          theme(axis.title = element_text(colour="grey20")) +
      theme(plot.title = element_text(color = "grey20", size = 12, face = "plain") )
      
      plot
      ggsave("new_graves_imputation_loocv.png", width = 18, height = 14, units = "cm", dpi = "print")    
  
        
    # Save final model
     write.csv(tidy(fit), "out_graves_area_pred_model.csv")  

    # Generate predictions and their standard errors for observations with missing burial data
      # estimates
        obs[, "new_graves_pred"] <- predict(fit, newdata = obs, type = "response", allow.new.levels = TRUE)
      # standard errors
          # for fixed effects models only
          if ( grepl("|", deparse1(formula(fit)) ) == FALSE ) {
          obs[, "new_graves_pred_se"] <- predict(fit, newdata = obs, type = "response", allow.new.levels = TRUE,
            se.fit = TRUE)[["se.fit"]]
          }
          # for mixed models
            # following version should run but not sure it is accurate; there is no method for SE prediction in glmer
          if ( grepl("|", deparse1(formula(fit)) ) == TRUE) {
            obs[, "new_graves_pred_se"] <- NA

            # mm <- model.matrix(~area_ln, obs)
            # f_pred <- function(.) exp(mm%*%fixef(.)) 
            # x2 <- bootMer(fit, nsim = 10, FUN = f_pred)
            # obs[, "new_graves_pred_se"] <- apply(x2$t, 2, sd)
          }


  #...................................   
  ## Use best estimate predictions wherever burial observations are missing
    # Attribute predicted values to missing observations
    obs[, "new_graves"] <- ifelse(is.na(obs[, "new_graves"]), round(obs[, "new_graves_pred"], 0), obs[, "new_graves"] )

    # Calculate total graves for missing observations, to the extent possible given new graves imputed above
      # process is iterative as newly imputed values in turn enable more imputation;
      # where both back- and forward-calculation are possible, a weighted average of both is applied
      
      # subset of observations with either graves, new graves or surface area non-missing
      x1 <- obs[! is.na(obs$graves) | ! is.na(obs$new_graves) | ! is.na(obs$area), 
        c("cemetery", "date", "graves", "new_graves")]
      x1[, "graves_imputed"] <- FALSE
        # sort
          x1 <- x1[order(x1[, "cemetery"], x1[, "date"]), ]
      
      # control parameters for loop
      control1 <- TRUE
      control2 <- 0
          
      # for each cemetery...
      for (i in sort(unique(x1$cemetery)) ) {

        # reset control parameter 1 (if FALSE for any cemetery after loop, replication stops)
        control1 <- TRUE # needs to be TRUE at start
        
        # while there are still potentially values to impute...
        while (control1 == TRUE) {
          # select data from cemetery
          x2 <- subset(x1, cemetery == i)
          
          # progress statement
          print(paste("now working on cemetery...", i, sep = "") )
          
          # reset control parameter 2
          control2 <- 0
            
          # for each of the observations...
          for (j in 1:nrow(x2) ) {
            x3 <- NA; x4 <- NA; x5 <- NA; x6 <- NA; x7 <- NA;
            
            # if the graves value is (still) missing...
            if ( is.na(x2[j, "graves"]) ) {
                  
              # calculate based on previous observation within the cemetery, if there is one
              if (j != 1) {x3 <- ifelse(is.na(x2[j-1, "graves"]), NA, x2[j-1, "graves"] + x2[j, "new_graves"] ) }
                  
              # calculate based on next observation within the cemetery, if there is one
              if (j != nrow(x2) ) {x4 <- ifelse(is.na(x2[j+1, "graves"]), NA, x2[j+1, "graves"] - x2[j+1, "new_graves"] ) }
                  
              # take a weighted mean of the alternative values based on previous and next observations
              if (! is.na(x3) | ! is.na(x4) ) {
                # calculate weights: whichever of the two is not available takes a zero weight
                x5 <- ifelse(is.na(x3), 0, x3 - x2[j-1, "graves"] )
                x6 <- ifelse(is.na(x4), 0, x2[j+1, "graves"] - x4 )
                x7 <- c(x5 / (x5 + x6), x6 / (x5 + x6) )
                x7[is.na(x7)] <- 0 # final weights
                # take weighted mean
                x1[x1$cemetery == i & x1$date == x2[j, "date"], "graves"] <- round(weighted.mean(c(x3, x4), x7), 0 )
                x1[x1$cemetery == i & x1$date == x2[j, "date"], "graves_imputed"] <- TRUE
                # update control parameter
                control2 <- control2 + 1
                
              }
            }
          }
        # update control parameter after each loop of 'while' statement
        if (control2 == 0) {control1 <- FALSE} else {control1 <- TRUE}
          
        }
    }

      
    # Attribute imputed values to missing observations
      x1 <- subset(x1, graves_imputed)
      x1[, "graves_imputed_values"] <- x1[, "graves"] 
      obs <- merge(obs, x1[, c("date", "cemetery", "graves_imputed_values", "graves_imputed")], by = c("date", "cemetery"), all.x = TRUE)
      obs[, "graves"] <- ifelse(is.na(obs[, "graves"]), obs[, "graves_imputed_values"], obs[, "graves"] )
      
    # Update starting number of graves variable
      obs <- obs[order(obs[, "cemetery"], obs[, "date"]), ]
      x1 <- c()
      for (i in sort(unique(obs$cemetery)) ) {
        x1 <- c(x1, min(subset(obs, cemetery == i)$graves, na.rm = TRUE) )
      }
      x1 <- data.frame(sort(unique(obs$cemetery)), x1)
      colnames(x1) <- c("cemetery", "graves_start")
      obs <- merge(obs[, colnames(obs) != "graves_start"], x1, by = "cemetery", x.all = TRUE)

    # Update new graves since the last observation
      obs <- obs[order(obs[, "cemetery"], obs[, "date"]), ]
      x1 <- c()
      for (i in sort(unique(obs$cemetery)) ) {
        x2 <- subset(obs, cemetery == i)
        x1 <- c(x1, NA, diff (x2$graves ) )

      }
      obs[, "new_graves"] <- x1
