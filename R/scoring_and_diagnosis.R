#' Calculate PTSD total score
#'
#' Calculates the totals score from PTSD symptoms
#'
#' #' @description
#' Calculates the total PCL-5 (PTSD Checklist for DSM-5) score by summing all
#' 20 symptom scores. The total score ranges from 0 to 80, with higher scores
#' indicating greater symptom severity.
#'
#' @param data A dataframe containing standardized PCL-5 item scores (output of
#'   rename_ptsd_columns). Each symptom should be scored on a 0-4 scale where:
#'   * 0 = Not at all
#'   * 1 = A little bit
#'   * 2 = Moderately
#'   * 3 = Quite a bit
#'   * 4 = Extremely
#'
#' @returns A dataframe with all original columns plus an additional column "total"
#'   containing the sum of all 20 symptom scores (range: 0-80)
#'
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
#' # Calculate total scores
#' scores_with_total <- calculate_ptsd_total(sample_data)
#' print(scores_with_total$total)
#'
calculate_ptsd_total <- function(data) {
  data %>%
    dplyr::mutate(total = rowSums(dplyr::select(data, .data$symptom_1:.data$symptom_20)))
}


#' Determine PTSD diagnosis based on DSM-5 criteria using non-binarized scores
#'
#' @description
#' Determines whether DSM-5 diagnostic criteria for PTSD are met based on PCL-5
#' item scores, using the original non-binarized values (0-4 scale).
#'
#' @details
#' The function applies the DSM-5 diagnostic criteria for PTSD:
#' * Criterion B (Intrusion): At least 1 symptom ≥ 2 from items 1-5
#' * Criterion C (Avoidance): At least 1 symptom ≥ 2 from items 6-7
#' * Criterion D (Negative alterations in cognitions and mood):
#'   At least 2 symptoms ≥ 2 from items 8-14
#' * Criterion E (Alterations in arousal and reactivity):
#'   At least 2 symptoms ≥ 2 from items 15-20
#'
#' A symptom is considered present when rated 2 (Moderately) or higher.
#'
#' @param data A dataframe that can be either:
#'   * Output of rename_ptsd_columns(): 20 columns named symptom_1 to symptom_20
#'   * Output of calculate_ptsd_total(): 21 columns including symptom_1 to
#'     symptom_20 plus a 'total' column
#'
#'   Each symptom should be scored on a 0-4 scale where:
#'   * 0 = Not at all
#'   * 1 = A little bit
#'   * 2 = Moderately
#'   * 3 = Quite a bit
#'   * 4 = Extremely
#'
#' @returns A dataframe with all original columns (including 'total' if present)
#'   plus an additional column "PTSD_Diagnosis" containing TRUE/FALSE values
#'   indicating whether DSM-5 diagnostic criteria are met
#'
#' @export
#'
#' @examples
#' # Example with output from rename_ptsd_columns
#' sample_data1 <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' colnames(sample_data1) <- paste0("symptom_", 1:20)
#' diagnosed_data1 <- create_ptsd_diagnosis_nonbinarized(sample_data1)
#'
#' # Check diagnosis results
#' table(diagnosed_data1$PTSD_Diagnosis)
#'
#' # Example with output from calculate_ptsd_total
#' sample_data2 <- calculate_ptsd_total(sample_data1)
#' diagnosed_data2 <- create_ptsd_diagnosis_nonbinarized(sample_data2)
#'
#' # Check diagnosis results
#' table(diagnosed_data2$PTSD_Diagnosis)
#'
create_ptsd_diagnosis_nonbinarized <- function(data) {
  criteria <- list(
    A = rowSums(data[, paste0("symptom_", 1:5)] >= 2) >= 1,
    B = rowSums(data[, paste0("symptom_", 6:7)] >= 2) >= 1,
    C = rowSums(data[, paste0("symptom_", 8:14)] >= 2) >= 2,
    D = rowSums(data[, paste0("symptom_", 15:20)] >= 2) >= 2
  )

  data$PTSD_Diagnosis <- Reduce(`&`, criteria)
  return(data)
}





#' Summarize PTSD scores and diagnoses
#'
#' @description
#' Creates a summary of PCL-5 total scores and PTSD diagnoses, including mean
#' total score, standard deviation, and number of positive diagnoses.
#'
#' @details
#' This function calculates key summary statistics for PCL-5 data:
#' * Mean total score (severity indicator)
#' * Standard deviation of total scores (variability in severity)
#' * Count of positive PTSD diagnoses (prevalence in the sample)
#'
#' @param data A dataframe containing at minimum:
#'   * A 'total' column with PCL-5 total scores (from calculate_ptsd_total)
#'   * A 'PTSD_Diagnosis' column with TRUE/FALSE values (from
#'     determine_ptsd_diagnosis)
#'
#' @returns A dataframe with one row containing:
#'   * mean_total: Mean PCL-5 total score
#'   * sd_total: Standard deviation of PCL-5 total scores
#'   * n_diagnosed: Number of positive PTSD diagnoses
#'
#' @export
#'
#' @importFrom dplyr summarise
#' @importFrom stats sd
#' @importFrom magrittr %>%
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   total = sample(0:80, 100, replace = TRUE),
#'   PTSD_Diagnosis = sample(c(TRUE, FALSE), 100, replace = TRUE)
#' )
#'
#' # Generate summary statistics
#' summary_stats <- summarize_ptsd(sample_data)
#' print(summary_stats)
#'
summarize_ptsd <- function(data) {
  data %>%
    dplyr::summarise(
      mean_total = mean(.data$total),
      sd_total = stats::sd(.data$total),
      n_diagnosed = sum(.data$PTSD_Diagnosis)
    )
}
