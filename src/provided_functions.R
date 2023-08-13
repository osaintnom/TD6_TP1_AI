# Load necessary libraries
library(rpart)
library(Metrics)
library(caret)
library(foreach)
library(doParallel)

#' Load a data frame from a CSV file.
#'
#' This function reads data from a CSV file, performs basic data validation,
#' and returns a prepared data frame for further analysis.
#'
#' @param filepath The path to the CSV file.
#' @param dataset_name A character string specifying the name of the dataset.
#' @param var_to_predict A character string specifying the name of the variable to predict.
#' @return A list containing the prepared data frame, dataset name, and target variable name.
#'
#' @details This function loads data from a CSV file into a data frame. It performs
#' initial checks to ensure that all column classes are valid ("integer", "factor", or "numeric").
#' The specified variable to predict is coerced into a factor, which is a common practice for
#' categorical target variables.
#'
load_df <- function(filepath, dataset_name, var_to_predict) {
  # Read the CSV file into a data frame
  data_df <- read.table(filepath,
                        header=TRUE, sep=",", na.strings="",
                        stringsAsFactors=TRUE)

  # Check if all column classes are valid
  column_classes <- sapply(data_df, class)
  if (any(!column_classes %in% c("integer", "factor", "numeric"))) {
    stop("At least one variable is neither 'integer', 'factor', nor 'numeric'.")
  }

  # Force the variable to predict to be a factor
  data_df[[var_to_predict]] <- factor(paste(var_to_predict, data_df[[var_to_predict]], sep="_"))

  return(list(data_df=data_df,
              dataset_name=dataset_name,
              var_to_predict=var_to_predict))
}

#' Preprocess data by handling missing values, one-hot encoding, etc.
#'
#' This function performs preprocessing tasks on the input data frames such as handling
#' missing values, one-hot encoding, imputing missing values, treating missing values as new levels
#' in factors, and more.
#'
#' @param var_to_predict The name of the target variable to predict.
#' @param train_df Training data frame.
#' @param val_df Validation data frame.
#' @param prop_NAs Proportion of missing values to generate at random in each attribute.
#' @param impute_NAs Logical indicating whether to impute missing values.
#' @param treat_NAs_as_new_levels Logical indicating whether to treat NAs as new levels in factors.
#' @param do_ohe Logical indicating whether to perform one-hot encoding.
#' @param discretize Logical indicating whether to discretize continuous variables.
#' @param n_bins Number of bins for discretizing numeric variables (if `discretize` is TRUE).
#' @param ord_to_numeric Logical indicating whether to convert ordered factors to numeric values (if `discretize` is TRUE).
#' @param prop_switch_y Proportion of target variable values to switch if specified.
#' @return A list containing the preprocessed training and validation data frames.
#'
#' @details This function preprocesses the input data frames by performing the following tasks:
#' - Adding an observation ID variable to both data frames.
#' - Separating input features and the target variable.
#' - Generating missing values in the data if specified.
#' - Discretizing numeric variables if `discretize` is TRUE.
#' - Imputing missing values if specified.
#' - Treating missing values as new levels in factors if specified.
#' - Performing one-hot encoding if specified.
#' - Switching target variable values if `prop_switch_y` is greater than 0.
#' - Combining the target variable back into the data frames.
#'
preprocess_data <- function(var_to_predict, train_df, val_df,
                            prop_NAs, impute_NAs, treat_NAs_as_new_levels,
                            do_ohe, discretize, n_bins, ord_to_numeric,
                            prop_switch_y) {

  # Add a obs id value to both datasets
  if ("obs_id_value" %in% colnames(train_df)) {
    stop("obs_id_value is already a variable name in the data")
  }
  train_df$obs_id_value <- 1:nrow(train_df)
  val_df$obs_id_value <- 1:nrow(val_df)

  # Separating input features and target variable from training and validation data
  X_train <- train_df[, setdiff(colnames(train_df), var_to_predict)]
  y_train <- train_df[, c("obs_id_value", var_to_predict)]
  X_val <- val_df[, setdiff(colnames(train_df), var_to_predict)]
  y_val <- val_df[, c("obs_id_value", var_to_predict)]
  rm(train_df, val_df)

  # Generating missing values in the data if specified
  if (prop_NAs > 0) {
    for (v in setdiff(colnames(X_train), "obs_id_value")) {
      num_to_sample <- ceiling(nrow(X_train) * prop_NAs)
      X_train[sample(1:nrow(X_train), num_to_sample), v] <- NA

      num_to_sample <- ceiling(nrow(X_val) * prop_NAs)
      X_val[sample(1:nrow(X_val), num_to_sample), v] <- NA
    }
  }

  # Discretize numeric variables
  if (discretize == TRUE) {
    no_variance <- nearZeroVar(X_train, freqCut=nrow(X_train) - 1)
    if (length(no_variance) > 0) {
      X_train <- X_train[, -no_variance]
      X_val <- X_val[, -no_variance]
    }
    for (v in setdiff(colnames(X_train), "obs_id_value")) {
      if (is.numeric(X_train[, v]) == TRUE) {
        cut_points <- unique(quantile(X_train[, v],
                                      prob=seq(from=0, to=1, length.out=n_bins + 1),
                                      na.rm=TRUE))
        cut_points[1] <- -Inf
        cut_points[length(cut_points)] <- Inf
        X_train[, v] <- cut(X_train[, v], breaks=cut_points)
        X_val[, v] <- cut(X_val[, v], breaks=cut_points)
        if (ord_to_numeric == TRUE) {
          X_train[, v] <- as.numeric(X_train[, v])
          X_val[, v] <- as.numeric(X_val[, v])
        }
      }
    }
  }

  # Imputing missing values if specified
  if (impute_NAs == TRUE) {
    for (v in setdiff(colnames(X_train), "obs_id_value")) {
      if (is.factor(X_train[,v]) == TRUE) {
        level_counts <- table(X_train[,v])
        level_with_high_freq <- names(level_counts)[which.max(level_counts)]
        X_train[is.na(X_train[v]),v] <- level_with_high_freq
        X_val[is.na(X_val[v]),v] <- level_with_high_freq
      } else {
        v_mean <- mean(X_train[,v], na.rm=TRUE)
        X_train[is.na(X_train[v]),v] <- v_mean
        X_val[is.na(X_val[v]),v] <- v_mean
      }
    }
  }

  # Treating missing values as new levels in factors if specified
  if (treat_NAs_as_new_levels == TRUE) {
    for (v in setdiff(colnames(X_train), "obs_id_value")) {
      if (is.factor(X_train[,v]) == TRUE) {
        levels(X_train[,v]) <- c(levels(X_train[,v]), "is_NA")
        X_train[is.na(X_train[,v]), v] <- "is_NA"

        levels(X_val[,v]) <- c(levels(X_val[,v]), "is_NA")
        X_val[is.na(X_val[,v]), v] <- "is_NA"
      }
    }
  }

  # Performing one-hot encoding if specified
  if (do_ohe == TRUE) {
    no_variance <- nearZeroVar(X_train, freqCut=nrow(X_train) - 1)
    if (length(no_variance) > 0) {
      X_train <- X_train[, -no_variance]
      X_val <- X_val[, -no_variance]
    }
    encoder <- dummyVars(~ ., X_train)
    X_train <- as.data.frame(predict(encoder, newdata=X_train))
    X_val <- as.data.frame(predict(encoder, newdata=X_val))
  }

  # Switch training labels values
  if (prop_switch_y > 0) {
    y_to_switch <- sample(y_train$obs_id_value, ceiling(nrow(y_train) * prop_switch_y))
    to_switch <- y_train$obs_id_value %in% y_to_switch
    y_numeric_new <- as.numeric(y_train[[var_to_predict]]) %% 2 + 1
    class_levels <- levels(y_train[[var_to_predict]])
    switched_val <- factor(sapply(y_numeric_new, function(x) class_levels[x]), levels=class_levels)
    y_train[to_switch, var_to_predict] <- switched_val[to_switch]
  }

  # Combining the target variable back to the data frames
  X_train <- merge(X_train, y_train, by="obs_id_value")
  X_train$obs_id_value <- NULL
  X_val <- merge(X_val, y_val, by="obs_id_value")
  X_val$obs_id_value <- NULL

  return(list(train_df=X_train, val_df=X_val))
}

#' Estimate the validation AUC for a decision tree.
#'
#' This function estimates the validation area under the receiver operating characteristic curve (AUC)
#' for a decision tree model. It performs repeated validation
#' and calculates the AUC for each repetition.
#'
#' @param var_to_predict The name of the target variable to predict.
#' @param tree_control Control parameters for the rpart decision tree.
#' @param data_df The data frame containing the data for modeling.
#' @param prop_val Proportion of observations to use as validation.
#' @param reps Number of repetitions for repeated validation.
#' @param preprocess_control A list containing preprocessing control options.
#'
#' @return A data frame containing the estimated AUC for each repetition,
#' along with the maximum depth used in the decision tree.
#'
#' @details
#' This function estimates the AUC for a decision tree model using repeated validation.
#' It follows these steps:
#' 1. Generate random splits for validation.
#' 2. Perform repeated validation with preprocessing.
#' 3. Fit a decision tree model for each repetition and calculate the AUC.
#' 4. Store the AUC values by repetition and maximum depth used.
#' 5. Combine the AUC results into a single data frame.
#'
rep_val_estimate <- function(var_to_predict, tree_control, data_df, prop_val, reps, preprocess_control) {

  # Initialize a list to store AUC values for each repetition
  exp_aucs <- list()

  # Initialize an index to keep track of the current position in the list
  i <- 1

  # Perform repeated validation
  for (r in 1:reps) {

    # Split the data into training and validation sets for the current partition
    val_index <- sample(1:nrow(data_df), ceiling(nrow(data_df) * prop_val))
    train_df <- data_df[-val_index,]
    val_df <- data_df[val_index,]

    # Preprocess data for the current partition
    processed_dfs <- preprocess_data(var_to_predict, train_df,
                                     val_df,
                                     preprocess_control$prop_NAs,
                                     preprocess_control$impute_NAs,
                                     preprocess_control$treat_NAs_as_new_levels,
                                     preprocess_control$do_ohe,
                                     preprocess_control$discretize,
                                     preprocess_control$n_bins,
                                     preprocess_control$ord_to_numeric,
                                     preprocess_control$prop_switch_y)

    # Fit decision tree and predict on the validation set
    trained_tree <- rpart(as.formula(paste(var_to_predict, " ~ .")),
                          data=processed_dfs$train_df,
                          control=tree_control)

    # Save effective tree depth
    tree_depth <- max(rpart:::tree.depth(as.numeric(rownames(trained_tree$frame))))

    # Predict probabilities for a class in the validation set
    val_preds <- predict(trained_tree, newdata=processed_dfs$val_df, type="prob")
    first_col_name <- colnames(val_preds)[1]
    val_preds <- val_preds[, first_col_name]

    # Convert the target variable to numeric (1 for the positive class, 0 for others)
    val_targets <- as.numeric(processed_dfs$val_df[, var_to_predict] == first_col_name)

    # Store AUC and the effective depth of the tree for the current repetition
    exp_aucs[[i]] <- data.frame(rep=r,
                                auc=Metrics::auc(val_targets, val_preds),
                                tree_depth=tree_depth)
    i <- i + 1
  }

  # Combine the individual AUC results into a single data frame
  exp_aucs <- do.call(rbind, exp_aucs)

  # Add a column to store the maximum depth used in the decision tree
  exp_aucs$maxdepth <- tree_control$maxdepth

  return(exp_aucs)
}

#' Estimate AUC for different tree depths using repeated validation and parallelization
#'
#' This function estimates the area under the receiver operating characteristic curve (AUC)
#' for decision trees of different depths using validation and parallelization. It performs
#' repeated validation for each tree depth and returns a data frame with AUC scores.
#'
#' @param data_to_pred A list containing the data frame, target variable name, and dataset name.
#' @param preprocess_control A list containing preprocessing control options.
#' @param max_maxdepth Maximum tree depth to try.
#' @param prop_val Proportion of observations to use as validation.
#' @param val_reps Number of repetitions for repeated validation.
#'
#' @return A data frame with AUC scores for different tree depths.
#'
#' @details
#' This function estimates the AUC scores for decision trees of varying depths using repeated
#' validation and parallel processing. It follows these steps:
#' 1. Extract the data frame and target variable name from the input list.
#' 2. Set up parallel processing using the doParallel package to speed up computation.
#' 3. For each tree depth from 1 to the specified maximum depth (`max_maxdepth`):
#'    - Create a decision tree control object (`tree_control`) with the current maximum depth.
#'    - Estimate AUC scores using the `rep_val_estimate` function with rigorous preprocessing.
#'    - Store the AUC scores for the current tree depth.
#' 4. Combine the AUC scores for different tree depths into a single data frame.
#'
est_auc_across_depths <- function(data_to_pred, preprocess_control, max_maxdepth, prop_val, val_reps) {
  data_df <- data_to_pred$data_df
  var_to_predict <- data_to_pred$var_to_predict

  # Set up parallel processing using doParallel package
  cl <- makeCluster(N_THREADS, outfile="")
  registerDoParallel(cl)

  # Suppress print statements on worker nodes
  clusterEvalQ(cl, suppressMessages(library(rpart)))
  clusterEvalQ(cl, suppressMessages(library(caret)))

  auc_across_depths <- foreach(md=1:max_maxdepth,
                               .inorder=FALSE,
                               .export=c("rep_val_estimate", "preprocess_data")) %dopar% {
    print(md)
    tree_control <- rpart.control(minsplit=2,
                                  minbucket=1,
                                  maxdepth=md,
                                  xval=0,
                                  cp=0)

    # Estimate AUC for the current tree depth
    auc_tmp <- rep_val_estimate(var_to_predict, tree_control, data_df,
                                prop_val, val_reps, preprocess_control)

    return(auc_tmp)
  }
  stopCluster(cl)

  # Combine the AUC scores for different tree depths into a data frame
  auc_across_depths <- do.call(rbind, auc_across_depths)
  auc_across_depths$dataset_name <- data_to_pred$dataset_name

  return(auc_across_depths)
}

#' Estimate AUC for different tree depths using repeated validation and no parallelization
#'
#' This function estimates the area under the receiver operating characteristic curve (AUC)
#' for decision trees of different depths using validation and parallelization. It performs
#' repeated validation for each tree depth and returns a data frame with AUC scores.
#'
#' @param data_to_pred A list containing the data frame, target variable name, and dataset name.
#' @param preprocess_control A list containing preprocessing control options.
#' @param max_maxdepth Maximum tree depth to try.
#' @param prop_val Proportion of observations to use as validation.
#' @param val_reps Number of repetitions for repeated validation.
#'
#' @return A data frame with AUC scores for different tree depths.
#'
#' @details
#' This function estimates the AUC scores for decision trees of varying depths using repeated
#' validation and parallel processing. It follows these steps:
#' 1. Extract the data frame and target variable name from the input list.
#' 2. Set up parallel processing using the doParallel package to speed up computation.
#' 3. For each tree depth from 1 to the specified maximum depth (`max_maxdepth`):
#'    - Create a decision tree control object (`tree_control`) with the current maximum depth.
#'    - Estimate AUC scores using the `rep_val_estimate` function with rigorous preprocessing.
#'    - Store the AUC scores for the current tree depth.
#' 4. Combine the AUC scores for different tree depths into a single data frame.
#'
est_auc_across_depths_no_par <- function(data_to_pred, preprocess_control, max_maxdepth, prop_val, val_reps) {
  data_df <- data_to_pred$data_df
  var_to_predict <- data_to_pred$var_to_predict

  auc_across_depths <- list()

  for (md in 1:max_maxdepth) {
    print(md)
    tree_control <- rpart.control(minsplit=2,
                                  minbucket=1,
                                  maxdepth=md,
                                  xval=0,
                                  cp=0)

    # Estimate AUC for the current tree depth
    auc_tmp <- rep_val_estimate(var_to_predict, tree_control, data_df,
                                prop_val, val_reps, preprocess_control)

    auc_across_depths[[md]] <- auc_tmp
  }

  # Combine the AUC scores for different tree depths into a data frame
  auc_across_depths <- do.call(rbind, auc_across_depths)
  auc_across_depths$dataset_name <- data_to_pred$dataset_name

  return(auc_across_depths)
}
