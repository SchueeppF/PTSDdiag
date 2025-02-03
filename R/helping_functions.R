#' Binarize PCL-5 symptom scores
#'
#' @description
#' Converts PCL-5 symptom scores from their original 0-4 scale to binary values
#' (0/1) based on the clinical threshold for symptom presence (≥2).
#'
#' @details
#' The function implements the standard clinical threshold for PTSD symptom
#' presence where:
#' * Scores of 0-1 ("Not at all" and "A little bit") → 0 (symptom absent)
#' * Scores of 2-4 ("Moderately" to "Extremely") → 1 (symptom present)
#'
#' @param data A dataframe containing exactly 20 columns with PCL-5 item scores
#'  (output of rename_ptsd_columns). Each symptom should be scored
#'   on a 0-4 scale where:
#'   * 0 = Not at all
#'   * 1 = A little bit
#'   * 2 = Moderately
#'   * 3 = Quite a bit
#'   * 4 = Extremely
#'
#'  Note: This function should only be used with raw symptom scores before
#'  calculating the total score, as it will convert all values in the dataframe
#'  to 0/1, which would invalidate any total score column if present.
#'
#' @returns A dataframe with the same structure as input but with all symptom
#'   scores converted to binary values:
#'   * 0 = Symptom absent (original scores 0-1)
#'   * 1 = Symptom present (original scores 2-4)
#'
#' @export
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' colnames(sample_data) <- paste0("symptom_", 1:20)
#'
#' # Binarize scores
#' binary_data <- binarize_data(sample_data)
#' table(binary_data) # Should only show 0s and 1s
#'
binarize_data <- function(data) {
  # Binarize values (0,1 -> 0; 2,3,4 -> 1)
  data[data <= 1] <- 0
  data[data >= 2] <- 1
  return(data)
}

#' Determine PTSD diagnosis based on DSM-5 criteria using binarized scores
#'
#' @description
#' Determines whether DSM-5 diagnostic criteria for PTSD are met using binarized
#' symptom scores (0/1) for PCL-5 items. This is an alternative to
#' determine_ptsd_diagnosis() that works with pre-binarized data.
#'
#' @details
#' The function applies the DSM-5 diagnostic criteria for PTSD using binary
#' indicators of symptom presence:
#' * Criterion B (Intrusion): At least 1 present symptom from items 1-5
#' * Criterion C (Avoidance): At least 1 present symptom from items 6-7
#' * Criterion D (Negative alterations in cognitions and mood):
#'   At least 2 present symptoms from items 8-14
#' * Criterion E (Alterations in arousal and reactivity):
#'   At least 2 present symptoms from items 15-20
#'
#' @param data A dataframe containing exactly 20 columns of PCL-5 item scores
#'   (output of rename_ptsd_columns) named symptom_1 to symptom_20. Each symptom
#'   should be scored on a 0-4 scale where:
#'   * 0 = Not at all
#'   * 1 = A little bit
#'   * 2 = Moderately
#'   * 3 = Quite a bit
#'   * 4 = Extremely
#'
#' Note: This function should only be used with raw symptom scores (output of
#' rename_ptsd_columns) and not with data containing a total score column, as
#' the internal binarization process would invalidate the total score.
#'
#' @returns A dataframe with a single column "PTSD_orig" containing TRUE/FALSE
#'   values indicating whether DSM-5 diagnostic criteria are met based on
#'   binarized scores
#'
#' @export
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' colnames(sample_data) <- paste0("symptom_", 1:20)
#'
#' # Get diagnosis using binarized approach
#' diagnosis_results <- create_ptsd_diagnosis_binarized(sample_data)
#' table(diagnosis_results$PTSD_orig)
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

  return(data.frame(PTSD_orig = ptsd_results))
}

#' Summarize changes in PTSD diagnostic metrics
#'
#' @description
#' Compares different PTSD diagnostic criteria by calculating diagnostic accuracy
#' metrics and changes in diagnosis status relative to a baseline criterion.
#'
#' #' @details
#' The function calculates multiple diagnostic metrics comparing each diagnostic
#' criterion to a baseline criterion (PTSD_orig):
#'
#' Basic counts:
#' * Number and percentage of diagnosed/non-diagnosed cases per criterion
#' * Number of newly diagnosed and newly non-diagnosed cases
#' * True positive and true negative cases
#'
#' Diagnostic accuracy metrics:
#' * Sensitivity: Proportion of true PTSD cases correctly identified
#' * Specificity: Proportion of non-PTSD cases correctly identified
#' * PPV (Positive Predictive Value): Probability that a positive diagnosis is correct
#' * NPV (Negative Predictive Value): Probability that a negative diagnosis is correct
#'
#' @param data A dataframe where:
#'   * Each column represents a different diagnostic criterion
#'   * Must include a column named "PTSD_orig" as the baseline criterion
#'   * Values are logical (TRUE/FALSE) indicating whether PTSD criteria are met
#'   * Each row represents one case/participant
#'
#' @returns A dataframe containing the following columns for each diagnostic criterion:
#'   * column: Name of the diagnostic criterion
#'   * diagnosed: Number of cases diagnosed as PTSD
#'   * non_diagnosed: Number of cases not diagnosed as PTSD
#'   * diagnosed_percent: Percentage of cases diagnosed
#'   * non_diagnosed_percent: Percentage of cases not diagnosed
#'   * newly_diagnosed: Cases diagnosed under new but not baseline criterion
#'   * newly_nondiagnosed: Cases diagnosed under baseline but not new criterion
#'   * true_positive: Cases diagnosed under both criteria
#'   * true_negative: Cases not diagnosed under either criterion
#'   * true_cases: Sum of true positives and true negatives
#'   * false_cases: Sum of newly diagnosed and newly non-diagnosed
#'   * sensitivity, specificity, ppv, npv: Standard diagnostic accuracy metrics
#'
#' @export
#'
#' @examples
#' # Create sample diagnostic data
#' set.seed(123)
#' n_cases <- 100
#' sample_data <- data.frame(
#'   PTSD_orig = sample(c(TRUE, FALSE), n_cases, replace = TRUE),
#'   PTSD_alt1 = sample(c(TRUE, FALSE), n_cases, replace = TRUE),
#'   PTSD_alt2 = sample(c(TRUE, FALSE), n_cases, replace = TRUE)
#' )
#'
#' # Calculate diagnostic metrics
#' diagnostic_metrics <- summarize_ptsd_changes(sample_data)
#'
summarize_ptsd_changes <- function(data) {
  # Initialize results dataframe
  summary_stats <- data.frame(
    column = names(data),
    diagnosed = colSums(data),
    non_diagnosed = colSums(!data),
    stringsAsFactors = FALSE
  )

  # Calculate changes compared to PTSD_orig
  baseline <- data$PTSD_orig
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

#' Create readable summary of PTSD diagnostic changes
#'
#' @description
#' Formats the output of summarize_ptsd_changes() into a more readable table
#' with proper labels and formatting of percentages and metrics.
#'
#' @details
#' Reformats the diagnostic metrics into a presentation-ready format:
#' * Combines counts with percentages for diagnosed/non-diagnosed cases
#' * Rounds diagnostic accuracy metrics to 4 decimal places
#' * Provides clear column headers for all metrics
#'
#' @param summary_stats A dataframe output from summarize_ptsd_changes()
#'   containing raw diagnostic metrics and counts
#'
#' @returns A formatted dataframe with the following columns:
#'   * Scenario: Name of the diagnostic criterion
#'   * Total Diagnosed: Count and percentage of diagnosed cases
#'   * Total Non-Diagnosed: Count and percentage of non-diagnosed cases
#'   * True Positive: Count of cases diagnosed under both criteria
#'   * True Negative: Count of cases not diagnosed under either criterion
#'   * Newly Diagnosed: Count of new positive diagnoses
#'   * Newly Non-Diagnosed: Count of new negative diagnoses
#'   * True Cases: Total correctly classified cases
#'   * False Cases: Total misclassified cases
#'   * Sensitivity, Specificity, PPV, NPV: Diagnostic accuracy metrics (4 decimals)
#'
#' @export
#'
#' @examples
#' # Using the output from summarize_ptsd_changes
#' n_cases <- 100
#' sample_data <- data.frame(
#'   PTSD_orig = sample(c(TRUE, FALSE), n_cases, replace = TRUE),
#'   PTSD_alt1 = sample(c(TRUE, FALSE), n_cases, replace = TRUE)
#' )
#'
#' # Generate and format summary
#' diagnostic_metrics <- summarize_ptsd_changes(sample_data)
#' readable_summary <- create_readable_summary(diagnostic_metrics)
#' print(readable_summary)
#'
create_readable_summary <- function(summary_stats) {
  data.frame(
    Scenario = summary_stats$column,
    `Total Diagnosed` = paste0(summary_stats$diagnosed,
                               " (", summary_stats$diagnosed_percent, "%)"),
    `Total Non-Diagnosed` = paste0(summary_stats$non_diagnosed,
                                   " (", summary_stats$non_diagnosed_percent, "%)"),
    `True Positive` = summary_stats$true_positive,
    `True Negative` = summary_stats$true_negative,
    `Newly Diagnosed` = summary_stats$newly_diagnosed,
    `Newly Non-Diagnosed` = summary_stats$newly_nondiagnosed,
    `True Cases` = summary_stats$true_cases,
    `False Cases` = summary_stats$false_cases,
    `Sensitivity` = round(summary_stats$sensitivity, 4),
    `Specificity` = round(summary_stats$specificity, 4),
    `PPV` = round(summary_stats$ppv, 4),
    `NPV` = round(summary_stats$npv, 4),
    check.names = FALSE
  )
}
