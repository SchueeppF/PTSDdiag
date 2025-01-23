#' Rename PTSD symptom columns
#'
#' Rename columns in the input data to standardize symptom names
#' (symptom_1 to symptom_20)
#'
#' @param data A dataframe containing 20 columns, one for each PTSD symptom
#'
#' @returns A dataframe with renamed columns
#' @export
#'
#' @importFrom dplyr %>% rename_with
#' @importFrom magrittr %>%
#'
rename_ptsd_columns <- function(data) {
  data %>%
      rename_with(~ paste0("symptom_", 1:20))
}
