#' Binarize data
#'
#' Symptoms with values of 0 or 1 are considered as absent, symptom values of ≥2
#' are considered as present
#'
#' @param data A dataframe with values between 0-4 for each of the 20 symptoms
#'
#' @returns A dataframe with 0 (=absent) or 1 (=present) for each of the 20 symptoms
#' @export
#'
binarize_data <- function(data) {
  # Binarize values (0,1 -> 0; 2,3,4 -> 1)
  data[data <= 1] <- 0
  data[data >= 2] <- 1
  return(data)
}


#' Determine PTSD Diagnosis (binarized)
#'
#' Determines PTSD on the basis of the binarized values for the 20 items
#'
#' @param data A dataframe with raw symptom scores of the 20 symptoms
#'
#' @returns A dataframe with one column
#' indicating whether diagnostic criteria are met
#' @export
#'
create_ptsd_diagnosis_binarized <- function(data) {
  check_ptsd_criteria <- function(symptoms) {
    criterion_1 <- any(symptoms[1:5] == 1)
    criterion_2 <- any(symptoms[6:7] == 1)
    criterion_3 <- sum(symptoms[8:14] == 1) >= 2
    criterion_4 <- sum(symptoms[15:20] == 1) >= 2
    return(criterion_1 & criterion_2 & criterion_3 & criterion_4)
  }

  # Binarize data
  binarized_data <- binarize_data(data)
  # Check PTSD criteria for each row
  ptsd_results <- apply(binarized_data, 1, check_ptsd_criteria)

  return(data.frame(PTSD_all = ptsd_results))
}

#' Summarize changes in diagnostic metrics
#' when comparing different diagnostic criteria
#'
#' @param data A dataframe with columns showing whether the diagnosis is
#' fulfilled under certain diagnostic criteria
#'
#' @returns Summary statistic for the different diagnostic criteria
#' @export
#'
summarize_ptsd_changes <- function(data) {
  # Initialize results dataframe
  summary_stats <- data.frame(
    column = names(data),
    diagnosed = colSums(data),
    non_diagnosed = colSums(!data),
    stringsAsFactors = FALSE
  )

  # Calculate changes compared to PTSD_all
  baseline <- data$PTSD_all
  # For each column
  for(col in names(data)) {
    current <- data[[col]]

    newly_diagnosed <- sum(!baseline & current)
    newly_nondiagnosed <- sum(baseline & !current)
    true_positive <- sum(baseline & current)
    true_negative <- sum(!baseline & !current)

    summary_stats$newly_diagnosed[summary_stats$column == col] <- newly_diagnosed
    summary_stats$newly_nondiagnosed[summary_stats$column == col] <- newly_nondiagnosed
    summary_stats$true_positive[summary_stats$column == col] <- true_positive
    summary_stats$true_negative[summary_stats$column == col] <- true_negative
    summary_stats$true_cases[summary_stats$column == col] <- true_positive + true_negative
    summary_stats$false_cases[summary_stats$column == col] <- newly_diagnosed + newly_nondiagnosed
    summary_stats$sensitivity[summary_stats$column == col] <- true_positive / (true_positive + newly_nondiagnosed)
    summary_stats$specificity[summary_stats$column == col] <- true_negative / (true_negative + newly_diagnosed)
    summary_stats$ppv[summary_stats$column == col] <- true_positive / (true_positive + newly_diagnosed)
    summary_stats$npv[summary_stats$column == col] <- true_negative / (true_negative + newly_nondiagnosed)
  }

  # Calculate percentages
  total_cases <- nrow(data)
  summary_stats$diagnosed_percent <- round(summary_stats$diagnosed / total_cases * 100, 2)
  summary_stats$non_diagnosed_percent <- round(summary_stats$non_diagnosed / total_cases * 100, 2)

  return(summary_stats)
}

#' Create readable summary of changes in diagnostic metrics when comparing
#' different diagnostic criteria
#'
#' @param summary_stats Resulting dataframe of function summarize_ptsd_changes
#' @param columns Specific columns to include in summary
#'
#' @returns A formatted dataframe with summary statistic for the
#' different diagnostic criteria
#' @export
#'
create_readable_summary <- function(summary_stats, columns = NULL) {
  if(is.null(columns)) {
    summary_subset <- summary_stats
  } else {
    summary_subset <- summary_stats[summary_stats$column %in% columns,]
  }

  readable_summary <- data.frame(
    Scenario = summary_subset$column,
    `Total Diagnosed` = paste0(summary_subset$diagnosed,
                               " (", summary_subset$diagnosed_percent, "%)"),
    `Total Non-Diagnosed` = paste0(summary_subset$non_diagnosed,
                                   " (", summary_subset$non_diagnosed_percent, "%)"),
    `True Positive` = summary_subset$true_positive,
    `True Negative` = summary_subset$true_negative,
    `Newly Diagnosed` = summary_subset$newly_diagnosed,
    `Newly Non-Diagnosed` = summary_subset$newly_nondiagnosed,
    `True Cases` = summary_subset$true_cases,
    `False Cases` = summary_subset$false_cases, `Sensitivity` = round(summary_subset$sensitivity, 4),
    `Specificity` = round(summary_subset$specificity, 4),
    `PPV` = round(summary_subset$ppv, 4),
    `NPV` = round(summary_subset$npv, 4)
  )
  return(readable_summary)
}
