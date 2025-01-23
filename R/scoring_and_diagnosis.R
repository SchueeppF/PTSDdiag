#' Calculate PTSD total score
#'
#' Calculates the totals score from PTSD symptoms
#'
#' @param data A dataframe (output of rename_ptsd_columns) containing values
#' between 0-4 for each PTSD item
#'
#' @returns A dataframe with an additional column "total" containing the
#' total score
#'
#' @export
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
calculate_ptsd_total <- function(data) {
  data %>%
    dplyr::mutate(total = rowSums(dplyr::select(data, .data$symptom_1:.data$symptom_20)))
}


#' Determine PTSD diagnosis (non-binarized)
#'
#' Determines PTSD diagnosis on the basis of the non-binarized values
#' of the 20 items
#'
#' @param data A dataframe with at least 20 columns
#' (named symptom_1 to symptom_20), for each symptom a value between 0-4
#'
#' @returns A dataframe with an additional column "PTSD_Diagnosis" telling
#' if the Diagnosis criteria are fulfilled
#'
#' @export
#'
determine_ptsd_diagnosis <- function(data) {
  criteria <- list(
    A = rowSums(data[, paste0("symptom_", 1:5)] >= 2) >= 1,
    B = rowSums(data[, paste0("symptom_", 6:7)] >= 2) >= 1,
    C = rowSums(data[, paste0("symptom_", 8:14)] >= 2) >= 2,
    D = rowSums(data[, paste0("symptom_", 15:20)] >= 2) >= 2
  )

  data$PTSD_Diagnosis <- Reduce(`&`, criteria)
  return(data)
}


#' Summarize PTSD scores and diagnosis
#'
#' Creates a summary of PTSD total scores and diagnoses
#'
#' @param data A dataframe containing PTSD total scores and diagnoses
#'
#' @returns A dataframe with summary statistics
#' @export
#'
#' @importFrom dplyr %>% summarise
#' @importFrom stats sd
#' @importFrom magrittr %>%
#'
summarize_ptsd <- function(data) {
  data %>%
    dplyr::summarise(
      mean_total = mean(.data$total),
      sd_total = stats::sd(.data$total),
      n_diagnosed = sum(.data$PTSD_Diagnosis)
    )
}
