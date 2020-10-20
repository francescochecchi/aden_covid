#..........................................................................................
### +++++++ ESTIMATION OF COVID-19 ATTRIBUTABLE MORTALITY IN ADEN, YEMEN (2020) +++++++ ###
#..........................................................................................

#..........................................................................................
## ------------------ PIECES OF R CODE NOT USED IN THE FINAL ANALYSIS- ----------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Sep 2020)
                                          # francesco.checchi@lshtm.ac.uk



  # #...................................
  # ## Function to perform K-fold cross-validation and, if desired, plot predictions vs. observations for each fold
  #   f_cv <- function(f_fit, f_obs, f_k_folds, f_plot) {
  #
  #     # select observations for which all the formula variables are non-missing
  #     f_obs <- f_obs[complete.cases(f_obs[, all.vars(formula(f_fit)) ] ), ]
  #
  #     # determine dependent variable
  #     f_dep <- all.vars(formula(f_fit))[1]
  #
  #     # determine number of folds if f_k_folds = NA (i.e. LOOCV case)
  #     if (is.na(f_k_folds) == TRUE) { x1 <- nrow(f_obs) }
  #     if (is.na(f_k_folds) == FALSE) { x1 <- f_k_folds }
  #
  #     # is this a least-squares model?
  #     f_ols <- FALSE
  #     if (class(f_fit)[1] == "lm") {f_ols <- TRUE}
  #     if ( typeof(f_fit) == "S4") {f_ols <- ifelse("family" %in% names(f_fit@resp), FALSE, TRUE) }
  #
  #     # shuffle dataset
  #     f_obs <- f_obs[sample(nrow(f_obs), nrow(f_obs), replace=FALSE), ]
  #
  #     # split data into K folds
  #       # remove a few rows so as to come up with a n row divisible by K
  #       f_obs <- f_obs[1:(floor(nrow(f_obs)/x1) * x1), ]
  #       # split
  #       folds <- split(f_obs, (0:(nrow(f_obs)-1) %/% (nrow(f_obs)/x1)))
  #
  #     # fit model on all the unfolded sets and track square residuals of model fit on each fold, as well as predictions for each fold
  #       # vector to hold squared residuals and other statistics
  #       errors <- c()
  #       aic <- c()
  #       # vector to hold observations and predictions for each fold
  #       observations <- c()
  #       predictions <- c()
  #
  #     for (i in 1:length(folds) ) {
  #       # progress statement
  #       print(paste("now on fold ", i, " of ", length(folds), sep = "") )
  #       # fit on all data but the fold
  #       if (f_ols == FALSE) { cv.fit <- update(f_fit, data = do.call("rbind", folds[-i])) }
  #       if (f_ols == TRUE) { cv.fit <- update(f_fit, data = do.call("rbind", folds[-i])) }
  #       # calculate squared residual of model when predicting fold data, add to other errors for single test observations
  #       x1 <- predict(cv.fit, newdata = folds[[i]], type = "response", allow.new.levels = TRUE)
  #       # update output
  #       observations <- c(observations, folds[[i]][, f_dep])
  #       predictions <- c(predictions, x1)
  #       errors <- c(errors , (folds[[i]][, f_dep] -  x1 )^2 )
  #       aic <- c(aic, AIC(cv.fit))
  #
  #     }
  #
  #     # return RMSE across all folds
  #     print("mean RMSE across all folds:")
  #     print(mean(errors, na.rm = TRUE)^0.5)
  #
  #     # return AIC across all folds
  #     print("mean AIC across all folds:")
  #     print(mean(aic, na.rm = TRUE))
  #
  #     # if plot is desired...
  #     if (f_plot == TRUE) {
  #       # prepare data
  #       x1 <- as.data.frame(cbind(observations, predictions))
  #       colnames(x1) <- c("observations", "predictions")
  #         # if on log scale, back-transform to linear
  #         if (grepl("ln", f_dep) ) {x1[, c("observations", "predictions")] <- exp(x1[, c("observations", "predictions")]) }
  #
  #       # plot
  #       plot <- ggplot(x1) +
  #         geom_point(aes(x = observations, y = predictions), size=2, colour = brewer_pal(palette = "Dark2")(2)[1]) +
  #         theme_bw() +
  #         scale_x_continuous("observed") +
  #         scale_y_continuous("predicted") +
  #         geom_abline(intercept = 0, slope = 1, colour = brewer_pal(palette = "Dark2")(2)[2] ) +
  #         theme(axis.title = element_text(colour="grey20")) +
  #        ggtitle(paste("accuracy of predictions on cross-validation; model to predict ", all.vars(formula(f_fit))[1] , sep="") ) +
  #        theme(plot.title = element_text(color = "grey20", size = 12, face = "plain") )
  #
  #       print("plot shows accuracy of predictions on cross-validation")
  #       print(plot)
  #
  #       # return plot data
  #       invisible(x1)
  #
  #     }
  #
  #   }
  #




  # #...................................
  # ## Function to fit a mixed general linear model and display clean results (using lme4 package)
  #   f_glmm_lme4 <- function(f_dep, f_preds, f_reff, f_offset, f_data, f_family) {
  #     # write the model formula
  #       form <- as.formula( paste(f_dep, " ~ ", paste(f_preds, collapse= " + "),
  #         ifelse(is.na(f_offset), "", paste(" + offset(log(", f_offset, ") )", sep = "") ), " + ", f_reff, sep="")  )
  #
  #     # select observations for which all the formula variables are non-missing or non-NA
  #     f_obs <- f_data[complete.cases(f_data[, all.vars(form) ] ), ]
  #
  #     # fit model depending on distributional assumption
  #     if (f_family == "nb") { fit <- tryCatch(glmer.nb(form, data = f_obs), error=function(er) {return(FALSE)} );
  #       if (fit == FALSE) {fit <- glmmTMB(form, data = f_obs, family=nbinom2) }
  #     }
  #       if (f_family != "nb") { fit <- glmer(form, data = f_obs, family = f_family) }
  #
  #     # return fit
  #       return(fit)
  #   }
  #





        # Also calculate total graves for these missing observations, to the extend possible given new graves imputed above
      # sort
      obs <- obs[order(obs[, "cemetery"], obs[, "date"]), ]
      # indices of data with some ground observations (graves OR area)
      x1 <- which(is.na(obs$graves) == FALSE | is.na(obs$area) == FALSE)
      # for each of the observations...
      for (i in 1:length(x1) ) {
        # if the graves value is actually missing...
        if ( is.na(obs[x1[i], "graves"]) ) {
          # calculate based on previous observation within the cemetery, if there is one
            # is there?
            x2 <- TRUE # yes
            if (x1[i] == 1) {x2 <- FALSE} # no (first observation in cemetery series)
            if (x1[i] != 1) {
              if ( x2 == TRUE & (obs[x1[i], "cemetery"] != obs[x1[i-1], "cemetery"] ) ) {x2 <- FALSE} # no (first observation in cemetery series)
              if ( x2 == TRUE & is.na(obs[x1[i-1], "graves"]) ) {x2 <- FALSE} # no (previous observation is also missing)
            }
            # if yes, calculate
            x3 <- NA
            if (x2 == TRUE) { x3 <- obs[x1[i-1], "graves"] + obs[x1[i], "new_graves"] }

          # calculate based on next observation within the cemetery, if there is one
            # is there?
            x2 <- TRUE # yes
            if (x1[i] == max(x1) ) {x2 <- FALSE} # no (last observation in cemetery series)
            if (x1[i] != max(x1) ) {
              if ( (x2 == TRUE) & (obs[x1[i], "cemetery"] != obs[x1[i+1], "cemetery"] ) ) {x2 <- FALSE} # no (last observation in cemetery series)
              if ( (x2 == TRUE) & (is.na(obs[x1[i+1], "graves"]) ) ) {x2 <- FALSE} # no (next observation is also missing)
            }
            # if yes, calculate
            x4 <- NA
            if (x2 == TRUE) { x4 <- obs[x1[i+1], "graves"] - obs[x1[i+1], "new_graves"] }

          # take a weighted mean of the alternative values based on previous or next observations
            # calculate weights: whichever of the two is not available takes a zero weight
            x5 <- ifelse(is.na(x3), 0, x3 - obs[x1[i-1], "graves"] )
            x6 <- ifelse(is.na(x4), 0, obs[x1[i+1], "graves"] - x4 )
            x7 <- c(x5 / (x5 + x6), x6 / (x5 + x6) )
            x7[is.na(x7)] <- 0
            # take mean
            obs[x1[i], "graves"] <- round(weighted.mean(c(x3, x4), x7), 0 )
          }
      }




   #...................................
  ## Function to perform K-fold cross-validation and, if desired, plot predictions vs. observations for each fold
    f_cv <- function(f_fit, f_obs, f_k_folds, f_plot) {

      # access dataset used for fit
      if (class(f_fit) == "lm") {f_obs <- fit$data}
      if (typeof(f_fit) == "S4") {f_obs <- model.frame(fit)}
      if (class(f_fit) == "glmmTMB") {f_obs <- model.frame(fit)}
      if ("gamlss" %in% class(f_fit)) {
        f_obs <- model.frame(f_fit) ;
        colnames(f_obs) <- all.vars(formula(f_fit))
        }
View(f_obs)
        # if there is an offset, need to reconstitute it
        x1 <- grep("offset", colnames(f_obs), TRUE)
        if (length(x1) > 0) {
          x2 <- ! all.vars(formula(f_fit)) %in% colnames(f_obs)
          colnames(f_obs)[x1] <- all.vars(formula(f_fit))[x2]
          f_obs[, x1] <- exp(f_obs[, x1])
        }

      # select observations for which all the formula variables are non-missing
      f_obs <- f_obs[complete.cases(f_obs[, all.vars(formula(f_fit)) ] ), ]

      # determine dependent variable
      f_dep <- all.vars(formula(f_fit))[1]

      # determine number of folds if f_k_folds = NA (i.e. LOOCV case)
      if (is.na(f_k_folds) == TRUE) { x1 <- nrow(f_obs) }
      if (is.na(f_k_folds) == FALSE) { x1 <- f_k_folds }

      # is this a least-squares model?
      f_ols <- FALSE
      if (class(f_fit) == "lm") {f_ols <- TRUE}
      if ( typeof(f_fit) == "S4") {f_ols <- ifelse("family" %in% names(f_fit@resp), FALSE, TRUE) }

      # shuffle dataset
      f_obs <- f_obs[sample(nrow(f_obs), nrow(f_obs), replace=FALSE), ]

      # split data into K folds
        # remove a few rows so as to come up with a n row divisible by K
        f_obs <- f_obs[1:(floor(nrow(f_obs)/x1) * x1), ]
        # split
        folds <- split(f_obs, (0:(nrow(f_obs)-1) %/% (nrow(f_obs)/x1)))

      # fit model on all the unfolded sets and track square residuals of model fit on each fold, as well as predictions for each fold
        # vector to hold squared residuals and other statistics
        errors <- c()
        aic <- c()
        # vector to hold observations and predictions for each fold
        observations <- c()
        predictions <- c()

      for (i in 1:length(folds) ) {
        # progress statement
        print(paste("now on fold ", i, " of ", length(folds), sep = "") )
        # fit on all data but the fold
        data_now <- do.call("rbind", folds[-1])

        if (f_ols == FALSE) { cv.fit <- update(f_fit, formula=formula(f_fit),  family=family(f_fit)[[1]], data = data_now) }
        if (f_ols == TRUE) { cv.fit <- update(f_fit, formula=formula(f_fit),  data = data_now) }
        # calculate squared residual of model when predicting fold data, add to other errors for single test observations
        x1 <- predict(cv.fit, newdata = folds[[i]], type = "response", allow.new.levels = TRUE)
        # update output
        observations <- c(observations, folds[[i]][, f_dep])
        predictions <- c(predictions, x1)
        errors <- c(errors , (folds[[i]][, f_dep] -  x1 )^2 )
        aic <- c(aic, AIC(cv.fit))

      }

      # return RMSE across all folds
      print("mean RMSE across all folds:")
      print(mean(errors, na.rm = TRUE)^0.5)

      # return AIC across all folds
      print("mean AIC across all folds:")
      print(mean(aic, na.rm = TRUE))

      # if plot is desired...
      if (f_plot == TRUE) {
        # prepare data
        x1 <- as.data.frame(cbind(observations, predictions))
        colnames(x1) <- c("observations", "predictions")
          # if on log scale, back-transform to linear
          if (grepl("ln", f_dep) ) {x1[, c("observations", "predictions")] <- exp(x1[, c("observations", "predictions")]) }

        # plot
        plot <- ggplot(x1) +
          geom_point(aes(x = observations, y = predictions), size=2, colour = brewer_pal(palette = "Dark2")(2)[1]) +
          theme_bw() +
          scale_x_continuous("observed") +
          scale_y_continuous("predicted") +
          geom_abline(intercept = 0, slope = 1, colour = brewer_pal(palette = "Dark2")(2)[2] ) +
          theme(axis.title = element_text(colour="grey20")) +
         ggtitle(paste("accuracy of predictions on cross-validation; model to predict ", all.vars(formula(f_fit))[1] , sep="") ) +
         theme(plot.title = element_text(color = "grey20", size = 12, face = "plain") )

        print("plot shows accuracy of predictions on cross-validation")
        print(plot)

        # return plot data
        invisible(x1)

      }

    }







  # #...................................
  # ## Try fitting a discrete count growth model (GLMM): use lme4 and glmmTMB packages
  #   # basic, linear (neg-bin)
  #   fit <- f_glmm("graves", c("time_base"), "(1 + time_base | cemetery_id)", NA, obs, "nbinom2" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "nbinom2" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #     #glmmTMB fits well (no warnings), reasonable fit graphically
  #
  #   # added growth, linear (Poisson)
  #   fit <- f_glmm("graves", c("time_base", "time_covid"), "(1 + time_base + time_covid | cemetery_id)", NA, obs, "poisson" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "poisson" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #     # problems with Hessian, but reasonable fit graphically
  #
  #   # piecewise, linear (Poisson)
  #   fit <- f_glmm("graves", c("time_piece1", "time_piece2"), "(time_piece1 + time_piece2 | cemetery_id)", NA, obs, "poisson" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "poisson" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #     # problems with Hessian, but reasonable fit graphically
  #
  #   # basic, linear with period as predictor
  #   fit <- f_glmm("graves", c("time_base", "period_covid"), "(1 + time_base | cemetery_id)", NA, obs, "poisson" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "poisson" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #     # fits fine, but inappropriate decoupling at convergence of two periods
  #
  #
  # #...................................
  # ## Try fitting a linear mixed growth model (LMM) - log the dependent
  #   # basic, linear
  #   fit <- f_lmm("log(graves)", c("time_base"), "(1 + time_base | cemetery_id)",  obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       #fits but bad diagnostics (high-influence values?)
  #
  #   # basic, quadratic
  #   fit <- f_lmm("log(graves)", c("time_base", "I(time_base^2)"), "(1 + time_base + I(time_base^2) | cemetery_id)",  obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       # fits but terrible fit
  #
  #   # added growth, linear
  #   fit <- f_lmm("log(graves)", c("time_base", "time_covid"), "(1 + time_base + time_covid | cemetery_id)", obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       # problems with fit, terrible fit graphically
  #
  #   # added growth, quadratic in COVID time
  #   fit <- f_lmm("log(graves)", c("time_base", "time_covid", "I(time_covid^2)"),
  #     "(1 + time_base + time_covid + I(time_covid^2) | cemetery_id)", obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       # problems with fit, terrible fit graphically
  #
  #   # piecewise, linear
  #   fit <- f_lmm("log(graves)", c("time_piece1", "time_piece2"), "(time_piece1 + time_piece2 | cemetery_id)", obs)
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       # fits but terrible fit
  #
  #   # piecewise, quadratic in COVID time
  #   fit <- f_lmm("log(graves)", c("time_piece1", "time_piece2", "I(time_piece2^2)"),
  #   "(1 + time_piece1 + time_piece2 + I(time_piece2^2) | cemetery_id)",  obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       # fits but terrible fit
  #
  #   # piecewise, quadratic in both baseline and COVID time
  #   fit <- f_lmm("log(graves)", c("time_piece1", "I(time_piece1^2)", "time_piece2", "I(time_piece2^2)"),
  #   "(1 + time_piece1 + I(time_piece1^2) + time_piece2 + I(time_piece2^2) | cemetery_id)",  obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #       # problems with fit, terrible fit
  #
  #   # basic, linear with period as predictor
  #   fit <- f_lmm("log(graves)", c("time_base", "period_covid"), "(1 + time_base | cemetery_id)", obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, TRUE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #     # fits, but causes a discontinuity in predictions when period shifts to epidemic
  #
  #   # basic, quadratic with period as predictor
  #   fit <- f_lmm("log(graves)", c("time_base", "I(time_base^2)", "period_covid"),
  #     "(1 + time_base + I(time_base^2) | cemetery_id)", obs )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, FALSE)
  #
  #   fit <- glmmTMB(formula(fit), data = obs, family = "gaussian" )
  #   f_do(fit, f_diag_ols, f_cv, k_folds, TRUE, TRUE)
  #   f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ) )
  #     # terrible fit

##########################################
####GAMM models

          # Basic growth model
    fit <- gamlss(graves ~ ps(time_base) + re(random = ~1 + time_base | cemetery_id),
      sigma.formula=~pb(time_base), data = x2, family = NBI)
    summary(fit)
    plot(fit)
    f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      # fits, but for some cemeteries first data point is poorly predicted: restrict to later time periods?



      # version 4: specify knot
      break_point <- days[which(days$date == date_knot), "time_base"]
      fit <- gamlss(graves ~ pb(time_base) + pb(time_base > break_point) + re(random = ~1 + time_base | cemetery_id),
        sigma.formula=~pb(time_base), data = x2, family = NBI)
      summary(fit)
      plot(fit)
      f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))

      # ad-hoc plot
        x3 <- model.frame(fit)
        x3[, "predictions"] <- fitted(fit)
        x3[, c("cemetery", "date", "period_covid")] <- obs[row.names(x3), c("cemetery", "date", "period_covid")]
        plot <- ggplot(x3) +
           geom_line(aes(x = date, y = predictions ), linetype = "dashed", size = 0.7,
             colour = brewer_pal(palette = "Dark2")(2)[2] ) +
           geom_point(aes(x = date, y = graves, colour = period_covid), size = 1.5) +
           geom_line(aes(x = date, y = predictions, colour = period_covid), size = 0.9 ) +
           scale_colour_manual(values = brewer_pal(palette = "Dark2")(2)) +
           scale_y_continuous("number of graves") +
           theme_bw() +
           facet_wrap(~cemetery, nrow=5, scales = "free_y") +
           guides(fill = FALSE) +
           theme(legend.position="bottom", legend.direction="horizontal") +
           scale_x_date("", minor_breaks=NULL, date_breaks="4 months", date_labels = "%b-%Y" ) +
           labs(colour = "Period:  ") +
           ggtitle("within-sample predictions (lines) versus observations (dots), by cemetery" ) +
           theme(legend.title = element_text(color="grey20", size=11),
                 strip.text.x = element_text(color="grey20", size=11),
                 legend.text = element_text(color="grey20", size=11),
                 axis.title.x = element_text(color="grey20", size=11),
                 axis.text.x = element_text(color = "grey20", size=10, angle=315, hjust=0, vjust=0),
                 axis.line.y = element_line(color = "grey20"),
                 axis.ticks.y = element_line(color = "grey20"),
                 axis.text.y = element_text(color = "grey20", size=11),
                 axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
                 plot.title = element_text(color = "grey20", size = 12, face = "plain", hjust = 0.5),
                 plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
                 )
      print(plot)

      date_range <- c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) )
      # now zoom in on desired region of plot
      x2 <- seq(date_range[1], date_range[2], by = 1)
      plot1 <- plot %+% subset(x3, date %in% x2 ) + scale_x_date("", date_breaks="1 month", date_labels = "%b-%Y" )
      print(plot1)



          # Piecewise model
      # version 1: nth-order polynomials
      fit <- gamlss(graves ~ poly(time_piece1, 2) + poly(time_piece2, 2)
        + re(random = ~1 + time_piece1 + time_piece2 | cemetery_id),
        sigma.formula=~pb(time_piece1), data = x2, family = NBI)
      summary(fit)
      plot(fit)
      f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
        # no fit

      # version 2: smoothing parameters
      fit <- gamlss(graves ~ cs(time_piece1) + cs(time_piece2)
        + re(random = ~1 + time_piece1 + time_piece2 | cemetery_id),
        sigma.formula=~cs(time_piece1) + cs(time_piece2), data = x2, family = NBI)
      summary(fit)
      plot(fit)
      f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
        # no fit with either cs or pb


        # version 2: smoothing parameters
      fit <- gamlss(graves ~ pb(time_piece1) + pb(time_piece2)
        + re(random = ~1 + time_piece1 + time_piece2 | cemetery_id),
        sigma.formula=~pb(time_piece1) + pb(time_piece2), data = x2, family = NBI)
      summary(fit)
      plot(fit)
      f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
        # no fit with either cs or pb




    # For each potential epidemic start date...
    for (i in 1:length(date_knot_options)) {

      # recreate time variables
      obs <- obs[order(obs[, "cemetery"], obs[, "date"]), ]
        # for added growth model
          # forward version - epidemic time
          x1 <- rep(c(rep(0, times = (date_knot_options[i] - date_start) ),
                      seq(1, as.integer(date_end - date_knot_options[i] + 1), by = 1 )
                      ),
                    times = length(unique(obs$cemetery))
                   )
          obs[, "time_covid"] <- x1

          # backward version - epidemic time
          x1 <- rep(c(rep(-as.integer(date_end - date_knot_options[i]), times = (date_knot_options[i] - date_start) ),
                      seq(-as.integer(date_end - date_knot_options[i]), 0, by = 1 )
                      ),
                    times = length(unique(obs$cemetery))
                   )
          obs[, "time_covid_rev"] <- x1

          # knot version - baseline time
          x1 <- rep(c(seq(-as.integer(date_knot_options[i] - date_start), 0, by = 1 ),
                      seq(1, as.integer(date_end - date_knot_options[i]), by = 1)
                      ),
                    times = length(unique(obs$cemetery))
                   )
          obs[, "time_base_knot"] <- x1

        # for piecewise regression
          x1 <- rep(c(seq(as.integer(date_start - date_knot_options[i]), 0, by = 1),
                      rep(0, times = as.integer(date_end - date_knot_options[i]) )
                      ),
                    times = length(unique(obs$cemetery))
                   )
          obs[, "time_piece1"] <- x1

          x1 <- rep(c(rep(0, times = as.integer(date_knot_options[i] - date_start + 1)),
                      seq(0, as.integer(date_end - date_knot_options[i] - 1), by = 1)
                      ),
                    times = length(unique(obs$cemetery))
                   )
          obs[, "time_piece2"] <- x1

        # epidemic period
          obs[, "period_covid"] <- ifelse(obs$date < date_knot_options[i], "baseline", "epidemic")
          obs[, "period_covid"] <- as.factor(obs$period_covid)

        # refit model
          # select data
          x1 <- c("graves", "time_base", "cemetery_id", "time_covid", "time_base_rev", "time_covid_rev",
            "time_base_knot", "time_piece1", "time_piece2")
          x2 <- obs[complete.cases(obs[, x1]), x1]

          # fit model
          fit <- gamlss(graves ~ pb(time_base_rev, df = 2) + pb(time_covid_rev, df = 2)
            + re(random = ~1 + time_base_rev | cemetery_id),
            sigma.formula=~pb(time_base_rev) + pb(time_covid_rev), data = x2, family = NBI)
          summary(fit)
          plot(fit)
          f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))

    }


####VARIOUS GAMLSS VERSIONS

    # # version 1: nth-order polynomials
      # fit <- gamlss(graves ~ poly(time_base, 2) + poly(time_covid, 2)
      #   + re(random = ~1 + time_base | cemetery_id),
      #   sigma.formula=~pb(time_base) + pb(time_covid), data = x2, family = NBI)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #   # no fit if offset is used
      #
      # # version 2: smoothing parameters
      # fit <- gamlss(graves ~ cs(time_base, spar = 0.3) + cs(time_covid, spar = 0.3)
      #   + re(random = ~1 + time_base | cemetery_id),
      #   sigma.formula=~pb(time_base), data = x2, family = NBI)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #     # no fit if offset is used
      #
      # # version 3: cubic spline
      # fit <- gamlss(graves ~ pb(time_base) + pb(time_covid)
      #   + re(random = ~1 + time_base | cemetery_id),
      #   sigma.formula=~pb(time_base), data = x2, family = NBI)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #     # no fit if offset is used

      # fit <- gamlss(graves ~ poly(time_base_knot, 4) + poly(time_covid_rev, 3)
      #   + re(random = ~1 + time_base_knot | cemetery_id),
      #   sigma.formula=~pb(time_base_knot) + pb(time_covid_rev), data = x2, family = NBI)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #
      # fit <- gamlss(graves ~ pb(time_base_knot, df=4) + cs(time_covid_rev)
      #   + re(random = ~1 + time_base_knot | cemetery_id),
      #   sigma.formula=~pb(time_base_knot) + pb(time_covid_rev), data = x2, family = NBI)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #
      #
      #
      # # version 3: polynomials but all time backwards
      # fit <- gamlss(graves ~ poly(time_base_rev, 2) + poly(time_covid_rev, 4)
      #   + re(random = ~1 + time_base_rev | cemetery_id),
      #   sigma.formula=~pb(time_base_rev) + pb(time_covid_rev), data = x2, family = NBI)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #     # fits




      # fit_cv <- gamlss(graves ~ pb(time_base_rev, df = 2) + pb(time_covid_rev, df = 2)
      #   + re(random = ~1 + time_base_rev | cemetery_id), K.fold = 10,
      #   sigma.formula=~pb(time_base_rev) + pb(time_covid_rev), data = x2, family = NBI)
      #
          # MOST PROMISING: KEEP TIME BASE AS IS FOR SURE, TIME COVID ALSO LOOKS GOOD

      # # Trying mcgv package: (poor fit)
      # fit2 <- gam(graves ~ s(time_base_rev, bs = "bs") + s(time_covid_rev, bs = "bs") +
      #     s(time_base_rev, cemetery_id, bs = "re" ),
      #   data = x2, family = "nb", method = "GCV.Cp")
      # f_plot_gof(fit2, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))



      # # version 4: smoothing parameters but all time backwards
      # fit <- gamlss(graves ~ cs(time_base_rev, spar = 0.5) + cs(time_covid_rev, spar = 0.5)
      #   + re(random = ~1 + time_base_rev | cemetery_id),
      #   sigma.formula=~pb(time_base_rev) + pb(time_covid_rev), data = x2, family = NBI)
      #
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #     # fits: OK

      # # version 5: cubic spline but all time backwards
      # fit <- gamlss(graves ~ pb(time_base_rev) + cs(time_covid_rev, spar = 0.1)
      #   + re(random = ~1 + time_base_rev | cemetery_id),
      #   sigma.formula=~pb(time_base_rev) + pb(time_covid_rev), data = x2, family = NBI)
      # refit(fit)
      # summary(fit)
      # plot(fit)
      # f_plot_gof(fit, obs, c(as.Date("2020-01-01"), as.Date(max(obs$date, na.rm = TRUE)) ))
      #     # no fit if offset is used
      # # f_cv(fit, k_folds, TRUE)



          # Help to select the right model family and fitting method
    lms(graves, time_base, data = obs_fit, families = c("PO", "NBI", "NBII"))
      # suggests best to use NBI family and penalised B-splines (pb)
