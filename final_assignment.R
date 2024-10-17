# Check for the needed packages and install if not present on
# the current system.
pkgs <- rownames(installed.packages())
if (!("optparse" %in% pkgs)) install.packages(
  "optparse", repos = "https://cloud.r-project.org"
)
if (!("brms" %in% pkgs)) install.packages(
  "brms", repos = "https://cloud.r-project.org"
)
if (!("dplyr" %in% pkgs)) install.packages(
  "dplyr", repos = "https://cloud.r-project.org"
)
if (!("ggplot2" %in% pkgs)) install.packages(
  "ggplot2", repos = "https://cloud.r-project.org"
)
if (!("log4r" %in% pkgs)) install.packages(
  "log4r", repos = "https://cloud.r-project.org"
)
if (!("tidyverse" %in% pkgs)) install.packages(
  "tidyverse", repos = "https://cloud.r-project.org"
)
if (!("ggcorrplot" %in% pkgs)) install.packages(
  "ggcorrplot", repos = "https://cloud.r-project.org"
)
if (!("broom" %in% pkgs)) install.packages(
  "broom", repos = "https://cloud.r-project.org"
)
if (!("tidybayes" %in% pkgs)) install.packages(
  "tidybayes", repos = "https://cloud.r-project.org"
)
if (!("parallelly" %in% pkgs)) install.packages(
  "parallelly", repos = "https://cloud.r-project.org"
)
if (!("mgcv" %in% pkgs)) install.packages(
  "mgcv", repos = "https://cloud.r-project.org"
)
if (!("randomForest" %in% pkgs)) install.packages(
  "randomForest", repos = "https://cloud.r-project.org"
)
if (!("caret" %in% pkgs)) install.packages(
  "caret", repos = "https://cloud.r-project.org"
)
if (!("doParallel" %in% pkgs)) install.packages(
  "doParallel", repos = "https://cloud.r-project.org"
)
if (!("doSNOW" %in% pkgs)) install.packages(
  "doSNOW", repos = "https://cloud.r-project.org"
)
if (!("foreach" %in% pkgs)) install.packages(
  "foreach", repos = "https://cloud.r-project.org"
)
if (!("parallel" %in% pkgs)) install.packages(
  "parallel", repos = "https://cloud.r-project.org"
)
if (!("rsample" %in% pkgs)) install.packages(
  "rsample", repos = "https://cloud.r-project.org"
)
if (!("projpred" %in% pkgs)) install.packages(
  "projpred", repos = "https://cloud.r-project.org"
)
if (!("doRNG" %in% pkgs)) install.packages(
  "doRNG", repos = "https://cloud.r-project.org"
)
if (!("reshape2" %in% pkgs)) install.packages(
  "reshape2", repos = "https://cloud.r-project.org"
)

# Load the packages and suppress the incoming warnings
# and other messages that get logged out.
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(brms))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(log4r))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(tidybayes))
suppressPackageStartupMessages(library(parallelly))
suppressPackageStartupMessages(library(mgcv))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(doSNOW))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(projpred))
suppressPackageStartupMessages(library(doRNG))
suppressPackageStartupMessages(library(reshape2))

# Create a logging module for better viewing of messages
# and various other alerts.
logger <- logger(appenders = console_appender(logfmt_log_layout()))
info(logger, "Starting processing")

#' Utility function for unregistering parallel computing
#' when setting up and tearing down a dopar.
#'
#' No parameters and no return values.
unregister_dopar <- function() {
  info(
    logger,
    msg = "clearing out dopar variables"
  )
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

#' Utility function to create all figures needed for
#' the data analysis.
#'
#' @param path (character) the path to the dataset
#' @param output (character) the output directory
#' @return (data.frame) the cleaned data
create_data_analysis <- function(path, output) {
  housing <- get(load(path))

  # Ensure data is clean of nulls
  pre_drop <- nrow(housing)
  cleaned_housing <- housing %>% na.omit()
  post_drop <- nrow(cleaned_housing)
  info(
    logger,
    paste("dropped ", pre_drop - post_drop, " rows with null values", sep = "")
  )

  # we remove `medv` == 50 because the original authors of the dataset
  # believe that there is censoring happening in the data where these
  # values might actualy > 50
  # https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html)
  info(
    logger,
    msg = "cutting off `medv` == 50"
  )
  censored_housing <- cleaned_housing[cleaned_housing$medv != 50, ]
  post_censor <- nrow(censored_housing)
  info(
    logger,
    paste(
      "dropped ",
      post_drop - post_censor,
      " rows with `medv` == 50",
      sep = ""
    )
  )

  info(
    logger,
    msg = "generating data analysis plots ..."
  )
  analysis_folder <- "analysis"
  dir.create(file.path(output, analysis_folder), showWarnings = FALSE)

  info(
    logger,
    msg = "testing linearity ..."
  )
  qq <- qqnorm(censored_housing$medv)
  linearity <- ggplot(data = data.frame(qq), aes(x = x, y = y)) +
    geom_point() +
    ggtitle("Q-Q Plot (`medv`)")
  ggsave(
    paste(
      output,
      analysis_folder,
      "qqplot.png",
      sep = "/"
    ),
    plot = linearity
  )

  for (col in colnames(censored_housing)) {
    if (col == "chas") {
      next
    }
    info(
      logger,
      msg = paste("creating ", col, " boxplot", sep = "")
    )
    bplot <- ggplot(data = censored_housing, aes_string(y = col)) +
      geom_boxplot() +
      scale_x_discrete()
    ggsave(
      paste(
        output,
        analysis_folder,
        paste(col, "_boxplot.png", sep = ""),
        sep = "/"
      ),
      plot = bplot
    )

    info(
      logger,
      msg = paste("creating ", col, " histogram", sep = "")
    )
    hplot <- ggplot(data = censored_housing, aes_string(y = col)) +
      geom_histogram(bins = 20)
    ggsave(
      paste(
        output,
        analysis_folder,
        paste(col, "_density.png", sep = ""),
        sep = "/"
      ),
      plot = hplot
    )
  }

  info(
    logger,
    msg = "creating a correlation matrix plot"
  )
  corr_plot <- ggcorrplot(
    cor(subset(censored_housing, select = -c(chas))),
    hc.order = TRUE,
    lab = TRUE
  )
  ggsave(
    paste(
        output,
        analysis_folder,
        "correlation_plot.png",
        sep = "/"
      ),
      plot = corr_plot
  )

  info(
    logger,
    msg = "plotting some example highly important linear interactions"
  )
  rm_medv <- ggplot(censored_housing, aes(x = rm, y = medv)) +
    geom_point() +
    labs(
      x = "average rooms",
      y = "median value of home",
      title = "Housing Prices in Boston (Avg. Rooms vs. Median Value)"
    )
  ggsave(
    paste(
      output,
      analysis_folder,
      "rm_medv.png",
      sep = "/"
    ),
    plot = rm_medv
  )
  lstat_medv <- ggplot(censored_housing, aes(x = lstat, y = medv)) +
    geom_point() +
    labs(
      x = "% lower status of population",
      y = "median value of home",
      title = "Housing Prices in Boston (% Low Status vs. Median Value)"
    )
  ggsave(
    paste(
      output,
      analysis_folder,
      "lstat_medv.png",
      sep = "/"
    ),
    plot = lstat_medv
  )
  tax_medv <- ggplot(censored_housing, aes(x = tax, y = medv)) +
    geom_point() +
    labs(
      x = "property value tax per $10,000",
      y = "median value of home",
      title = "Housing Prices in Boston (Property Tax vs. Median Value)"
    )
  ggsave(
    paste(
      output,
      analysis_folder,
      "tax_medv.png",
      sep = "/"
    ),
    plot = tax_medv
  )
  return(censored_housing)
}

#' Function that train, test splits the data based on a holdout
#' percentage.  This is used to generate priors and assess the
#' overall model fit.
#'
#' @param data (data.frame) the data to split
#' @param holdout (double) the percentage of data to holdout
#' @return list(train=data.frame, test=data.frame)
train_test_split <- function(data, holdout = 0.1) {
  if (holdout <= 0 || holdout >= 1) {
    error(
      logger,
      msg = "holdout percentage should not be < 0 or > 1",
      holdout = holdout
    )
  } else if (holdout > 0.3) {
    warn(
      logger,
      msg = "be careful on splitting the data larger than 30%",
      holdout = holdout
    )
  }

  info(
    logger,
    msg = "train test splitting"
  )
  data_split <- initial_split(data, prop = 1 - holdout)

  train <- training(data_split)
  test  <- testing(data_split)
  return(list(train = train, test = test))
}

#' Generate a formula and priors based on a vector of
#' variables.
#'
#' @param best_variables (vector) a vector of variables
#' @return list(formula=, priors=c())
create_priors_and_formula <- function(best_variables) {
  form <- "medv ~ "
  priors <- c()
  for (x in seq_along(best_variables)) {
    if (best_variables[x] %in% c("chas", "rad", "lstat", "rm")) {
      if (best_variables[x] == "chas") {
        priors <- append(
          priors,
          set_prior(
            "student_t(3, 0, 50)",
            class = "b",
            coef = "chas1"
          )
        )
      } else {
        priors <- append(
          priors,
          set_prior(
            "student_t(3, 0, 50)",
            class = "b",
            coef = best_variables[x]
          )
        )
      }
    } else {
      priors <- append(
        priors,
        set_prior(
          "student_t(3, 0, 50)",
          class = "b",
          coef = paste("s", best_variables[x], "_1", sep = "")
        )
      )
    }
    if (x == 1) {
      if (best_variables[x] %in% c("chas", "rad", "lstat", "rm")) {
        form <- paste(form, best_variables[x], "+", sep = " ")
      } else {
        form <- paste(
          form, "s(", best_variables[x], ") + ", sep = ""
        )
      }
    } else if (x == length(best_variables)) {
      if (best_variables[x] %in% c("chas", "rad", "lstat", "rm")) {
        form <- paste(form, best_variables[x], sep = " ")
      } else {
        form <- paste(
          form, "s(", best_variables[x], ")", sep = ""
        )
      }
    } else {
      if (best_variables[x] %in% c("chas", "rad", "lstat", "rm")) {
        form <- paste(form, best_variables[x], "+", sep = " ")
      } else {
        form <- paste(
          form, "s(", best_variables[x], ") + ", sep = ""
        )
      }
    }
  }
  info(
    logger,
    msg = "created formula and priors",
    formula = form,
    priors = paste(
      sapply(
        priors$prior, paste, collapse = ":"
      ),
      collapse = " "
    )
  )
  return(list(formula = form, priors = priors))
}

#' Use a projection based method to assess the feature
#' importance for modelling. We are restricting the space
#' that the feature importance looks to improve speed and
#' overall performance.
#'
#' @param data (data.frame) the data to fit the model
#' @param output (character) path to the output
#' @return list(features=c(), priors=c())
feature_selection_projection <- function(data, output, seed, draws, cores) {
  info(
    logger,
    msg = "fitting model for feature selection",
    cores = cores,
    seed = seed
  )
  feature_model <- brm(
    formula = "medv ~ (.)",
    family = "gaussian",
    data = data,
    seed = seed,
    iter = draws,
    cores = cores
  )

  # Create a cluster of `cores` size
  cl <- makeCluster(cores, outfile = "")
  registerDoSNOW(cl)

  info(
    logger,
    msg = "running parallel feature selection",
    cores = cores,
    seed = seed
  )
  # limiting the space to speed up computation time
  cvs <- cv_varsel(
    feature_model,
    refit_prj = FALSE,
    nclusters = 5,
    parallel = TRUE,
    seed = seed
  )

  # Kill the cluster
  stopCluster(cl)
  info(
    logger,
    msg = "parallel feature selection run completed!"
  )
  unregister_dopar()

  varsel_plot <- plot(cvs, stats = c("elpd", "rmse"))

  info(
    logger,
    msg = "generating feature selection plots ..."
  )
  feature_folder <- "feature_selection"
  dir.create(file.path(output, feature_folder), showWarnings = FALSE)

  suggested_size <- suggest_size(cvs)
  best_variables <- ranking(cvs)$fulldata[1:suggested_size]

  png(
    filename = paste(
      output,
      feature_folder,
      "varsel_projection_plot.png",
      sep = "/"
    )
  )
  plot(varsel_plot)
  dev.off()

  info(
    logger,
    msg = "found best subset of features (projection)",
    features = best_variables
  )

  output <- create_priors_and_formula(best_variables)
  return(output)
}

#' Use recursive feature elimination for feature selection
#' to determine features to use for modelling.
#'
#' @param data (data.frame) the data to fit the model
#' @param output (character) path to the output
#' @return list(features=c(), priors=c())
feature_selection_rfe <- function(data, output) {
  info(
    logger,
    msg = "running rfe feature selection"
  )
  rfe_control <- rfeControl(functions = rfFuncs, method = "cv")
  results <- rfe(
    x = subset(data, select = -c(medv)),
    y = data$medv,
    sizes = seq_len(ncol(subset(data, select = -c(medv)))),
    rfeControl = rfe_control
  )

  feature_folder <- "feature_selection"
  dir.create(file.path(output, feature_folder), showWarnings = FALSE)

  png(
    filename = paste(
      output,
      feature_folder,
      "rfe_selection_plot.png",
      sep = "/"
    )
  )
  plot(results, type = c("g", "o"))
  dev.off()

  suggested_size <- results$optsize
  selected_vars <- results$variables
  best_variables <- results$control$functions$selectVar(
    selected_vars,
    suggested_size
  )

  info(
    logger,
    msg = "found best subset of features (rfe)",
    features = best_variables
  )
  output <- create_priors_and_formula(best_variables)
  return(output)
}

#' Utility function used by the parallel call to run a single model
#' on one single core.  This will run one model with a set of priors
#' for a certain number of draws on a specific seed and then return
#' back that model.
#'
#' @param specification_list (list) with a model formula and prior list
#'                           takes the form of list(formula="", priors=c())
#' @param draws (int) the number of draws to run the sampling for
#' @param data (list(train=train, test=test)) the input data to model
#' @param seed (int) the seed to run the modelling on
#' @return list(fitted_model=BRMS_model, loo=loo_output)
run_single_model <- function(specification_list, draws, data, seed) {
  info(
    logger,
    msg = "starting model run",
    formula = specification_list$formula,
    priors = paste(
      sapply(
        specification_list$priors$prior, paste, collapse = ":"
      ),
      collapse = " "
    ),
    seed = seed,
    draws = draws
  )
  temp <- brm(
    bf(specification_list$formula),
    data = data$train,
    family = "gaussian",
    seed = seed,
    cores = 1,
    iter = draws,
    prior = specification_list$priors,
    control = list(adapt_delta = 0.95),
    sample_prior = "yes"
  )
  loo_output <- loo::loo(temp)
  return(list(fitted_model = temp, loo = loo_output))
}

#' Code to generate the diagnostics and plots.  This will also generate
#' the out of sample RMSE and related plots.
#'
#' @param model (list) the fitted model
#' @param data (data.frame) the data to use for predictions
#' @param output (character) the directory to save the diagnostics to
#' @return NULL
generate_diagnostics <- function(model, data, output) {
  diagnostics_folder <- "diagnostics"
  dir.create(file.path(output, diagnostics_folder), showWarnings = FALSE)

  info(
    logger,
    msg = "generating trace plots"
  )
  trace_plots <- mcmc_plot(model$fitted_model, type = "trace")
  ggsave(
    paste(
      output,
      diagnostics_folder,
      paste(model$name, "_trace_plot.png", sep = ""),
      sep = "/"
    ),
    bg = "white",
    plot = trace_plots
  )

  info(
    logger,
    msg = "generating autocorrelation plots"
  )
  auto_corr_plots <- mcmc_plot(model$fitted_model, type = "acf")
  ggsave(
    paste(
      output,
      diagnostics_folder,
      paste(model$name, "_auto_corr_plot.png", sep = ""),
      sep = "/"
    ),
    bg = "white",
    plot = auto_corr_plots
  )

  info(
    logger,
    msg = "generating coefficient estimate plots"
  )
  coefficient_estimates <- mcmc_plot(model$fitted_model)
  ggsave(
    paste(
      output,
      diagnostics_folder,
      paste(model$name, "_coeff_estimates.png", sep = ""),
      sep = "/"
    ),
    bg = "white",
    plot = coefficient_estimates
  )

  info(
    logger,
    msg = "generating posterior probability plot"
  )
  ppc <- pp_check(
    model$fitted_model,
    ndraws = 1000,
    type = "stat",
    stat = "mean"
  )
  ggsave(
    paste(
      output,
      diagnostics_folder,
      paste(model$name, "_ppc_mean.png", sep = ""),
      sep = "/"
    ),
    bg = "white",
    plot = ppc
  )

  info(
    logger,
    msg = "creating posterior and prior overlays"
  )
  prior_pulls <- data.frame(prior_draws(model$fitted_model))
  posterior_pulls <- as_draws_df(model$fitted_model)

  for (col in colnames(posterior_pulls)) {
    if (!(col %in% colnames(prior_pulls))) {
      next
    }
    frame <- cbind(select(prior_pulls, col), select(posterior_pulls, col))
    colnames(frame) <- c(
      paste(col, "_prior", sep = ""),
      paste(col, "_posterior", sep = "")
    )
    melted <- melt(frame)
    prior_post_comp_plot <- ggplot(
      melted,
      aes(x = value, fill = variable)
    ) + geom_density(alpha = 0.25) + xlim(-50, 50)
    ggsave(
      paste(
        output,
        diagnostics_folder,
        paste(model$name, "_", col, "_prior_post_comp.png", sep = ""),
        sep = "/"
      ),
      bg = "white",
      plot = prior_post_comp_plot
    )
  }

  conditional_plots <- plot(
    conditional_effects(model$fitted_model),
    points = TRUE,
    ask = FALSE
  )
  for (i in seq_along(names(conditional_plots))) {
    info(
      logger,
      msg = "generating conditional effect plots",
      effect = names(conditional_plots)[[i]]
    )
    temp_plot <- conditional_plots[[names(conditional_plots)[[i]]]]
    ggsave(
      paste(
        output,
        diagnostics_folder,
        paste(
          model$name,
          "_",
          names(conditional_plots)[[i]],
          "_conditional_effect.png", sep = ""),
        sep = "/"
      ),
      bg = "white",
      plot = temp_plot
    )
  }

  info(
    logger,
    msg = "creating the out of sample RMSE",
    name = model$name,
    seed = seed
  )
  predictions <- predict(model$fitted_model, newdata = data$test, seed = seed)
  preds <- data.frame(predictions)

  rmse <- sqrt(
    mean((data$test$medv - preds$Estimate) ^ 2)
  )

  info(
    logger,
    msg = "creating error bar plot with true values"
  )
  preds$idx <- row.names(preds)
  preds$true_value <- data$test$medv
  boundaries <- sum(preds$true_value >= preds$Q2.5 & preds$true_value <= preds$Q97.5) / nrow(preds)
  error_plot <- ggplot(preds, aes(x = idx, y = Estimate, color = "Mean")) +
    geom_point() +
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5, color="95% CI")) +
    theme(axis.text.x = element_blank(), legend.position = "right") +
    geom_point(data = preds, aes(x = idx, y = true_value, color = "True `medv`")) +
    labs(
        title = "Comparision of Predictions to True Values",
        y = "Median Value",
        x = "Data Point",
        colour = "Legend"
    )
  ggsave(
    paste(
      output,
      diagnostics_folder,
      paste(model$name, "_error_plot.png", sep = ""),
      sep = "/"
    ),
    bg = "white",
    plot = error_plot
  )
  return(list(rmse = rmse, within_ci = boundaries, name = model$name))
}

#' Psuedo grid search where one model is trained on one core
#' with many models being fitted in parallel.  This function
#' creates a doPar cluster and then hands off one model to
#' each member of the cluster.
#'
#' @param data (data.frame) the input data to model
#' @param draws (int) the number of draws to run the sampling for
#' @param holdout (double) the percentage to holdout for fitting
#'                         priors and assessing model performance
#' @param seed (int) the seed to run the modelling on
#' @param cores (int) the size of the cluster to create
#' @param output (string) the output directory to save the model to
#' @return list(
#'            fitted_model=BRMS_model,
#'            loo=loo_output,
#'            formula=formula,
#'            name=var_name_of_model
#'          )
model_search <- function(data, draws, holdout, seed, cores, output) {
  selected_features_projection <- feature_selection_projection(
    data,
    output,
    seed,
    draws / 2,
    cores
  )
  selected_features_rfe <- feature_selection_rfe(data, output)

  models <- list(
    base_linear = list(
      formula = "medv ~ rm + lstat + chas + rad",
      priors = c(
        set_prior("student_t(3, 0, 50)", class = "b", coef = "lstat"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "rm"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "chas1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "rad")
      )
    ),
    all_features = list(
      formula = "medv ~ s(crim) + s(zn) + s(indus) + chas + s(nox) + rm + s(age) + s(dis) + rad + s(tax) + s(ptratio) + lstat",
      priors = c(
        set_prior("student_t(3, 0, 50)", class = "b", coef = "scrim_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "szn_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "sindus_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "snox_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "rm"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "sage_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "sdis_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "rad"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "stax_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "sptratio_1"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "lstat"),
        set_prior("student_t(3, 0, 50)", class = "b", coef = "chas1")
      )
    ),
    feature_selected_projection = list(
      formula = selected_features_projection$formula,
      priors = selected_features_projection$priors
    ),
    feature_selected_rfe = list(
      formula = selected_features_rfe$formula,
      priors = selected_features_rfe$priors
    )
  )

  info(
    logger,
    msg = "creating train, test splits for the data"
  )
  train_split <- train_test_split(data)

  full_data <- list(
    train = train_split$train,
    test = train_split$test
  )

  indices <- list(full_data$train$idx, full_data$test$idx)
  truthy_index <- sapply(
    seq_along(indices),
    function(i) length(intersect(indices[[i]], unlist(indices[-i]))) < 1
  )
  if (!all(truthy_index)) {
    error(
      logger,
      msg = "found duplicate indices in the train, test split",
      overlaps = truthy_index
    )
    stop()
  }
  info(
    logger,
    msg = "no duplicate indices found in the train, test data"
  )

  info(
    logger,
    msg = "starting parallel run ...",
    cores = cores
  )

  # Create a cluster of `cores` size
  cl <- makeCluster(cores, outfile = "")
  registerDoSNOW(cl)

  # Utility function to print a progress bar
  pb <- txtProgressBar(max = length(models), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  # Hand off the needed libraries, functions, and
  # variables to each core for processing.
  all_output <- foreach(
    i = seq_along(models),
    .combine = c,
    .packages = c("brms", "log4r"),
    .options.snow = opts,
    .export = c("logger", "run_single_model")
  ) %dopar% {
    output <- run_single_model(
      models[[i]],
      draws,
      full_data,
      seed
    )
    return(list(list(
      "fitted_model" = output$fitted_model,
      "formula" = models[[i]]$formula,
      "name" = names(models)[i],
      "loo" = output$loo
    )))
  }

  # Kill the cluster
  stopCluster(cl)
  info(
    logger,
    msg = "parallel run completed!"
  )
  unregister_dopar()

  # Find the best fitted model
  fitted_models <- list()
  for (i in seq_along(all_output)) {
    fitted_models[[all_output[[i]]$name]] <- all_output[[i]]$loo
  }

  compared <- loo::loo_compare(fitted_models)
  info(logger, msg = "saving LOO comparisons")

  bayes_model_folder <- "bayes_models"
  dir.create(file.path(output, bayes_model_folder), showWarnings = FALSE)
  write.csv(compared, paste(output, bayes_model_folder, "loo.csv", sep = "/"))

  info(
    logger,
    msg = "saving models (bayes and priors)"
  )
  sum_stats_models <- data.frame()
  for (model in all_output) {
    bayes_path <- paste(
      output, bayes_model_folder, paste(model$name, ".rds", sep = ""),
      sep = "/"
    )
    saveRDS(model$fitted_model, file = bayes_path)

    sum_stats <- generate_diagnostics(model, full_data, output)
    sum_stats_models <- rbind(sum_stats_models, data.frame(sum_stats))
    info(
      logger,
      msg = "saving coefficients",
      name = model$name
    )
    coefs <- data.frame(fixef(model$fitted_model))
    coef_path <- paste(
      output, bayes_model_folder, paste(model$name, ".csv", sep = ""),
      sep = "/"
    )
    write.csv(coefs, coef_path)
  }
  info(
    logger,
    msg = "saving summary stats values"
  )
  sum_stats_path <- paste(
    output, bayes_model_folder, "summary_stats.csv",
    sep = "/"
  )
  write.csv(sum_stats_models, sum_stats_path)
}

#' Allow for this to be run as an `Rscript`.  An example
#' of running this script if the BostonHousing.RData is in your
#' current working directory:
#'
#' Rscript assignment_2.R --file BostonHousing.RData
#'
#' A further more complete example of the `Rscript` could
#' be something like:
#'
#' Rscript assignment_2.R --file BostonHousing.RData \
#'    --seed 42 \
#'    --draws 30000 \
#'    --output . \
#'    --cores 0 \
#'    --holdout 0.1
#'
#' Just a note on the default arguments - if cores is 0,
#' the program will then try to select the maximum number
#' of available cores times 0.75 rounded down. This leaves
#' some head room for the scheduler and not to pin the
#' system too much.
option_list <- list(
  make_option(
    c("-s", "--seed"), action = "store",
    type = "integer", default = 42,
    help = "the random seed to set for analysis"
  ),
  # see the README.md for further discussion on default draw value
  make_option(
    c("-d", "--draws"), action = "store",
    type = "integer", default = 10000,
    help = "the number of draws to run"
  ),
  make_option(
    c("-o", "--output"), action = "store",
    type = "character", default = ".",
    help = "the output directory to save the plotted files"
  ),
  make_option(
    c("-f", "--file"), action = "store",
    type = "character",
    help = "the input Rdata file"
  ),
  make_option(
    c("-c", "--cores"), action = "store",
    type = "integer", default = 0,
    help = "the number of cores to parallelize across"
  ),
  make_option(
    c("-g", "--holdout"), action = "store",
    type = "double", default = 0.1,
    help = "the holdout percentage"
  )
)

opt <- parse_args(OptionParser(
  option_list = option_list,
  description = "this is a way to run the code for Bayesian Data Analysis 
  final assignment as an Rscript."
))

# Seed for reproducibility
seed <- opt$seed
# Draws for the model to make
draws <- opt$draws
# Directory to save plots etc.
output <- opt$output
# The input file (RDS file)
file <- opt$file
# Cores for how parallel
cores <- opt$cores
# Holdout percentage
holdout <- opt$holdout

# Ensure seed is valid
if (seed %% 1 != 0 || seed < 0) {
  error(
    logger,
    msg = "seed must be a positive integer",
    seed = seed
  )
}
# Ensure that the cores is actually valid.
if (cores < 0) {
  error(
    logger,
    msg = "cores must be positive",
    cores = cores
  )
} else if (cores == 0) {
  cores <- as.integer(availableCores() * 0.75)
  if (cores < 1) {
    cores <- 1
  }
  info(
    logger,
    msg = "using multiple cores to run the model",
    cores = cores
  )
} else if (cores > availableCores()) {
  cores <- availableCores() - 1
  info(
    logger,
    msg = "not that many cores available -- pinning to 1 - total cores",
    cores = cores
  )
}

if (output == ".") {
  output <- getwd()
}

# Assert that draws is large enough
if (draws %% 1 != 0 || draws < 5000) {
  error(
    logger,
    msg = paste(
      "please use am integer larger than 5000, ",
      draws,
      " does not conform to this",
      sep = ""
    )
  )
  stop()
}

if (grepl("Rdata", file, fixed = TRUE)) {
  error(
    logger,
    msg = paste(
      "file should be an 'Rdata' file ",
      file,
      " does not conform to this",
      sep = ""
    )
  )
  stop()
}

# prevent a silly `Rplot.pdf` file from being created
pdf(NULL)
info(
  logger,
  msg = "setting seed",
  seed = seed
)
set.seed(seed)
cleaned_housing <- create_data_analysis(path = file, output = output)
model_search(
  data = cleaned_housing,
  draws = draws,
  seed = seed,
  cores = cores,
  output = output
)
info(
  logger,
  msg = "writing the session info"
)
writeLines(
  capture.output(sessionInfo()),
  paste(output, "sessionInfo.txt", sep = "/")
)
