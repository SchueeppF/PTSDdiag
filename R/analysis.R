#' Find best three six-symptom-combinations for non-hierarchical PTSD diagnosis
#'
#' Analyzes PTSD symptom data to find the three best six-symptom-combinations,
#' where at least four symptoms need to be present for diagnosis,
#' regardless of which cluster they belong to
#'
#' @param data A dataframe with 20 columns for the 20 symptoms with
#' non-binarized values (columns named symptom_1 to symptom_20)
#' @param score_by Method to select the best six-symptom-combinations
#' (by minimizing the "newly_nondiagnosed" or by minimizing the "false_cases")
#'
#' @returns A list containing:
#'  best combinations: List of three vectors containing the symptom numbers of best combinations
#'  diagnosis comparison: data frame comparing original and new diagnostic criteria
#'  summary: datatable with formatted summary statistics
#' @export
#'
#' @importFrom utils combn
#' @importFrom DT datatable
#'
#' @examples
#' # Create example data
#' ptsd_data <- data.frame(matrix(sample(0:4, 200, replace=TRUE), ncol=20))
#' names(ptsd_data) <- paste0("symptom_", 1:20)
#'
#' results <- analyze_best_six_symptoms_four_required(ptsd_data)
#'
#' # Access best symptom combinations
#' results$best_symptoms
#'
#' # View formatted summary table
#' results$summary
#'
#' # Access raw comparison data
#' results$diagnosis_comparison
#'
analyze_best_six_symptoms_four_required <- function(data, score_by = "false_cases") {
  # Validate scoring method
  valid_scoring <- c("false_cases", "newly_nondiagnosed")
  if (!score_by %in% valid_scoring) {
    stop("score_by must be one of: ", paste(valid_scoring, collapse = ", "))
  }

  # Get baseline results and binarize data
  baseline_results <- create_ptsd_diagnosis_binarized(data)$PTSD_all
  binarized_data <- binarize_data(data)

  # Helper function to find best combinations
  find_best_combinations <- function(combinations, binarized_data, baseline_results, score_by, get_diagnosis_fn) {
    top_combinations <- list(
      first = list(combination = NULL, score = -Inf, diagnoses = NULL),
      second = list(combination = NULL, score = -Inf, diagnoses = NULL),
      third = list(combination = NULL, score = -Inf, diagnoses = NULL)
    )

    for(combination in combinations) {
      current_diagnoses <- get_diagnosis_fn(binarized_data, combination)

      newly_diagnosed <- sum(!baseline_results & current_diagnoses)
      newly_nondiagnosed <- sum(baseline_results & !current_diagnoses)

      score <- if(score_by == "false_cases") {
        -(newly_diagnosed + newly_nondiagnosed)
      } else {
        -newly_nondiagnosed
      }

      if(score > top_combinations$first$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- top_combinations$first
        top_combinations$first <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$second$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$third$score) {
        top_combinations$third <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      }
    }

    return(top_combinations)
  }

  # Helper function for diagnosis
  get_diagnosis <- function(data, symptoms) {
    subset_data <- data[, paste0("symptom_", symptoms)]
    return(rowSums(subset_data) >= 4)  # At least 4 symptoms must be present
  }

  # Generate all possible combinations of 6 symptoms and find best ones
  all_symptoms <- 1:20
  combinations <- utils::combn(all_symptoms, 6, simplify = FALSE)

  top_combinations <- find_best_combinations(combinations, binarized_data, baseline_results, score_by, get_diagnosis)

  # Create comparison dataframe
  comparison_df <- data.frame(
    PTSD_all = baseline_results,
    sapply(1:3, function(i) top_combinations[[i]]$diagnoses)
  )
  names(comparison_df)[2:4] <- sapply(1:3, function(i) {
    paste0("symptom_", paste(top_combinations[[i]]$combination, collapse = "_"))
  })

  return(list(
    best_symptoms = lapply(1:3, function(i) top_combinations[[i]]$combination),
    diagnosis_comparison = comparison_df,
    summary = DT::datatable(
      create_readable_summary(
        summarize_ptsd_changes(comparison_df)
      ),
      options = list(scrollX = TRUE)
    )
  ))
}




#' Find best three six-symptom-combinations for hierarchical PTSD diagnosis
#'
#' Analyzes PTSD symptom data to find the three best six-symptom-combinations,
#' where at least four symptoms need to be present for diagnosis,
#' one from each cluster
#'
#' @param data A dataframe with 20 columns for the 20 symptoms with
#' non-binarized values (columns named symptom_1 to symptom_20)
#' @param score_by Method to select the best six-symptom-combinations
#' (by minimizing the "newly_nondiagnosed" or by minimizing the "false_cases")
#'
#' @returns A list containing:
#'  best combinations: List of three vectors containing the symptom numbers of best combinations
#'  diagnosis comparison: data frame comparing original and new diagnostic criteria
#'  summary: datatable with formatted summary statistics
#' @export
#'
#' @importFrom utils combn
#' @importFrom DT datatable
#'
#' @examples
#' # Create example data
#' ptsd_data <- data.frame(matrix(sample(0:4, 200, replace=TRUE), ncol=20))
#' names(ptsd_data) <- paste0("symptom_", 1:20)
#'
#' results <- analyze_best_six_symptoms_four_required_clusters(ptsd_data)
#'
#' # Access best symptom combinations
#' results$best_symptoms
#'
#' # View formatted summary table
#' results$summary
#'
#' # Access raw comparison data
#' results$diagnosis_comparison
analyze_best_six_symptoms_four_required_clusters <- function(data, score_by = "false_cases") {
  # Validate scoring method
  valid_scoring <- c("false_cases", "newly_nondiagnosed")
  if (!score_by %in% valid_scoring) {
    stop("score_by must be one of: ", paste(valid_scoring, collapse = ", "))
  }

  # Get baseline results and binarize data
  baseline_results <- create_ptsd_diagnosis_binarized(data)$PTSD_all
  binarized_data <- as.matrix(binarize_data(data))

  # Define clusters
  clusters <- list(
    cluster1 = 1:5,
    cluster2 = 6:7,
    cluster3 = 8:14,
    cluster4 = 15:20
  )

  # Create lookup array for faster cluster membership checking
  cluster_lookup <- integer(20)
  for(i in seq_along(clusters)) {
    cluster_lookup[clusters[[i]]] <- i
  }

  # Fast cluster representation check using lookup
  check_cluster_representation <- function(symptoms) {
    length(unique(cluster_lookup[symptoms])) == 4
  }

  # Helper function to find best combination
  find_best_combinations <- function(combinations, binarized_data, baseline_results, score_by, get_diagnosis_fn) {
    top_combinations <- list(
      first = list(combination = NULL, score = -Inf, diagnoses = NULL),
      second = list(combination = NULL, score = -Inf, diagnoses = NULL),
      third = list(combination = NULL, score = -Inf, diagnoses = NULL)
    )

    for(combination in combinations) {
      current_diagnoses <- get_diagnosis_fn(binarized_data, combination)

      newly_diagnosed <- sum(!baseline_results & current_diagnoses)
      newly_nondiagnosed <- sum(baseline_results & !current_diagnoses)

      score <- if(score_by == "false_cases") {
        -(newly_diagnosed + newly_nondiagnosed)
      } else {
        -newly_nondiagnosed
      }

      if(score > top_combinations$first$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- top_combinations$first
        top_combinations$first <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$second$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$third$score) {
        top_combinations$third <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      }
    }

    return(top_combinations)
  }

  # Helper function for diagnosis
  get_diagnosis <- function(data, symptoms) {
    subset_data <- data[, symptoms, drop = FALSE]
    symptom_counts <- rowSums(subset_data)
    sufficient_rows <- which(symptom_counts >= 4)

    result <- logical(nrow(data))

    if(length(sufficient_rows) > 0) {
      for(i in sufficient_rows) {
        present_symptoms <- symptoms[subset_data[i,] == 1]
        if(length(present_symptoms) >= 4) {
          result[i] <- check_cluster_representation(present_symptoms)
        }
      }
    }

    return(result)
  }

  # Generate valid combinations efficiently
  valid_combinations <- vector("list", 1000)  # Pre-allocate
  combination_count <- 0

  for(s1 in clusters$cluster1) {
    for(s2 in clusters$cluster2) {
      for(s3 in clusters$cluster3) {
        for(s4 in clusters$cluster4) {
          base <- c(s1, s2, s3, s4)
          remaining <- setdiff(1:20, base)
          pairs <- utils::combn(remaining, 2, simplify = FALSE)

          for(pair in pairs) {
            combination_count <- combination_count + 1
            if(combination_count > length(valid_combinations)) {
              length(valid_combinations) <- length(valid_combinations) * 2
            }
            valid_combinations[[combination_count]] <- sort(c(base, pair))
          }
        }
      }
    }
  }

  valid_combinations <- valid_combinations[1:combination_count]
  valid_combinations <- unique(valid_combinations)

  # Find best combinations
  top_combinations <- find_best_combinations(
    valid_combinations,
    binarized_data,
    baseline_results,
    score_by,
    get_diagnosis
  )

  # Create comparison dataframe
  comparison_df <- data.frame(
    PTSD_all = baseline_results,
    sapply(1:3, function(i) top_combinations[[i]]$diagnoses)
  )
  names(comparison_df)[2:4] <- sapply(1:3, function(i) {
    paste0("symptom_", paste(top_combinations[[i]]$combination, collapse = "_"))
  })

  return(list(
    best_symptoms = lapply(1:3, function(i) top_combinations[[i]]$combination),
    diagnosis_comparison = comparison_df,
    summary = DT::datatable(
      create_readable_summary(
        summarize_ptsd_changes(comparison_df)
      ),
      options = list(scrollX = TRUE)
    )
  ))
}


