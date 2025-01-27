#' Rename PTSD symptom (= PCL-5 item) columns
#'
#' @description
#' Standardizes column names in PCL-5 (PTSD Checklist for DSM-5) data by renaming
#' them to a consistent format (symptom_1 through symptom_20). This standardization
#' is essential for subsequent analyses using other functions in the package.
#'
#' @details
#' The function assumes the input data contains exactly 20 columns corresponding to
#' the 20 items of the PCL-5. The columns are renamed sequentially from symptom_1
#' to symptom_20, maintaining their original order. The PCL-5 items correspond to
#' different symptom clusters:
#' * symptom_1 to symptom_5: Intrusion symptoms (Criterion B)
#' * symptom_6 to symptom_7: Avoidance symptoms (Criterion C)
#' * symptom_8 to symptom_14: Negative alterations in cognitions and mood (Criterion D)
#' * symptom_15 to symptom_20: Alterations in arousal and reactivity (Criterion E)
#'
#' @param data A dataframe containing exactly 20 columns, where each column
#'   represents a PCL-5 item score. The scores should be on a 0-4 scale where:
#'   * 0 = Not at all
#'   * 1 = A little bit
#'   * 2 = Moderately
#'   * 3 = Quite a bit
#'   * 4 = Extremely
#'
#' @returns A dataframe with the same data but renamed columns following the pattern
#'   'symptom_1' through 'symptom_20'
#'
#' @export
#'
#' @importFrom dplyr rename_with
#' @importFrom magrittr %>%
#'
#' @examples
#' # Example with a sample PCL-5 dataset
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' renamed_data <- rename_ptsd_columns(sample_data)
#' colnames(renamed_data)  # Shows new column names
#'
rename_ptsd_columns <- function(data) {
  data %>%
      rename_with(~ paste0("symptom_", 1:20))
}
