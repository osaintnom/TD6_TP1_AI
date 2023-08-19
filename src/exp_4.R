set.seed(782233747)

# Load the necessary libraries for data analysis and visualization
library(ggplot2)  # For creating plots
library(dplyr)    # For data manipulation

# Constants and global variables
PARALLELIZE <- TRUE # Set the option for parallelization of computations
N_THREADS <- 30     # Define the number of threads for parallel processing
N_BINS <- 10        # Define the number of bins for discretization
RERUN_EXP <- TRUE   # Set the option to rerun the experiment

# Load provided functions
source("provided_functions.R")

#' Run an experiment to evaluate the performance of a predictive model under different conditions.
#'
#' @param datasets_to_pred A list of data frames, each containing a dataset to be predicted.
#' @param filepath The path to the file where the experiment results will be saved.
#' @return None (the experiment results are saved to a file).
#'
#' @details
#' This function iterates through the given datasets, imputation methods, and proportions
#' of missing data. For each combination, it configures the preprocessing options, performs
#' the experiment, and stores the results in a list. The list of results is then combined into
#' a single data frame, which is saved to the specified file.
#'
run_experiment <- function(datasets_to_pred, filepath) {
  exp_results <- list()  # Store experiment results
  i <- 1  # Initialize counter for experiment results
  
  # Iterate through different dataset, imputation, and proportion of missing values combinations
  for (dtp in datasets_to_pred) {
    for (prop_switch_y in seq(0, 0.5, 0.025)){
      print(c(dtp$dataset_name, prop_switch_y))
      
      # Configure preprocessing options based on imputation choice
      preprocess_control <- list(
        prop_NAs=0,
        impute_NAs=FALSE,
        treat_NAs_as_new_levels=FALSE,
        do_ohe=FALSE,
        discretize=FALSE,
        n_bins=N_BINS,
        ord_to_numeric=FALSE,
        prop_switch_y=prop_switch_y
      )
      
      
      # Perform the experiment for the current settings
      if (PARALLELIZE == TRUE) {
        res_tmp <- est_auc_across_depths(dtp, preprocess_control,
                                         max_maxdepth=30, prop_val=0.25,
                                         val_reps=30)
      } else {
        res_tmp <- est_auc_across_depths_no_par(dtp, preprocess_control,
                                                max_maxdepth=30, prop_val=0.25,
                                                val_reps=30)
      }
      
      res_tmp$prop_switch_y <- prop_switch_y
      exp_results[[i]] <- res_tmp
      rm(res_tmp)  # Clean up temporary result
      i <- i + 1  # Increment result counter
    }
  }
  
  # Combine experiment results into a single data frame
  exp_results <- do.call(rbind, exp_results)
  
  # Save experiment results to a file
  write.table(exp_results, filepath, row.names=FALSE, sep="\t")
}

#' Plot the results of the sample experiment using ggplot2.
#'
#' @param filename_exp_results The filename of the experiment results file.
#' @param filename_plot The filename to save the plot (e.g., "my_plot.png").
#' @param width The width of the plot in inches.
#' @param height The height of the plot in inches.
#' @return None (the plot is saved as an image file).
#'
#' @details
#' This function reads the experiment results, calculates the mean AUC values for different
#' experimental conditions, and generates a line plot using ggplot2. The plot displays the mean AUC
#' values against maximum tree depths, with different lines for different imputation methods and facets
#' for different datasets and proportions of missing data. The resulting plot is saved as the specified file.
#'
plot_exp_results <- function(filename_exp_results, filename_plot, width, height) {
  # Load experiment results
  exp_results <- read.table(filename_exp_results, header=TRUE, sep="\t")
  
  # Calculate mean AUC values for different groups of experimental results
  data_for_plot <- exp_results %>%
    group_by(dataset_name, prop_switch_y, maxdepth) %>%
    summarize(mean_auc=mean(auc), .groups='drop')
  
  #Agrupamos los valores de AUC promedios recien calculados
  data_for_plot_max <- data_for_plot %>%
    group_by(dataset_name, prop_switch_y) %>%
    summarize(max_mean_auc = max(mean_auc), .groups='drop')
  
  # Create a ggplot object for the line plot
  g <- ggplot(data_for_plot_max, aes(x=prop_switch_y, y=max_mean_auc)) +
    geom_line() +
    theme_bw() +
    xlab("Prop_switch_y") +
    ylab("MAX AUC (estimated through repeated validation)") +
    facet_grid(. ~ dataset_name, scales="free_y") +
    theme(legend.position="bottom",
          panel.grid.major=element_blank(),
          strip.background=element_blank(),
          panel.border=element_rect(colour="black", fill=NA))
  
  # Save the plot to a file
  ggsave(filename_plot, g, width=width, height=height)
}

# Load the datasets
datasets_to_pred <- list(
  load_df("./data/customer_churn.csv", "Churn", "churn"), # Source: https://archive.ics.uci.edu/dataset/563/iranian+churn+dataset
  load_df("./data/heart.csv", "Heart", "HeartDisease"), # Source: https://www.kaggle.com/datasets/arnabchaki/data-science-salaries-2023
  load_df("./data/transformacion.csv", "Obesity", "NObeyesdad")) #Source: https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition.

# Run the experiment
if (RERUN_EXP ==  TRUE) {
  run_experiment(datasets_to_pred, "./outputs/tables/exp_4.txt")
}

# Plot the experiment results
plot_exp_results( "./outputs/tables/exp_4.txt", "./outputs/plots/exp_4.jpg", width=5, height=4)