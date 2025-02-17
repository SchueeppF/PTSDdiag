#' Simulated PCL-5 (PTSD Checklist) Data
#'
#' A dataset containing simulated responses from 5,000 patients on the PCL-5
#' (PTSD Checklist for DSM-5). Each patient rated 20 PTSD symptoms on a scale
#' from 0 to 4.
#'
#' @format A data frame with 5,000 rows and 20 columns:
#' \describe{
#'   \item{S1}{Intrusive memories (0-4 scale)}
#'   \item{S2}{Recurring dreams (0-4 scale)}
#'   \item{S3}{Flashbacks (0-4 scale)}
#'   \item{S4}{Emotional distress to reminders (0-4 scale)}
#'   \item{S5}{Physical reactions to reminders (0-4 scale)}
#'   \item{S6}{Avoiding memories/thoughts/feelings (0-4 scale)}
#'   \item{S7}{Avoiding external reminders (0-4 scale)}
#'   \item{S8}{Trouble remembering important aspects (0-4 scale)}
#'   \item{S9}{Strong negative beliefs (0-4 scale)}
#'   \item{S10}{Distorted blame (0-4 scale)}
#'   \item{S11}{Persistent negative emotions (0-4 scale)}
#'   \item{S12}{Decreased interest in activities (0-4 scale)}
#'   \item{S13}{Feeling distant from others (0-4 scale)}
#'   \item{S14}{Trouble experiencing positive emotions (0-4 scale)}
#'   \item{S15}{Irritability/aggression (0-4 scale)}
#'   \item{S16}{Risk-taking behavior (0-4 scale)}
#'   \item{S17}{Being "superalert" (0-4 scale)}
#'   \item{S18}{Heightened startle reaction (0-4 scale)}
#'   \item{S19}{Difficulty concentrating (0-4 scale)}
#'   \item{S20}{Sleep problems (0-4 scale)}
#' }
#' @details
#' The symptoms are rated on a 5-point scale:
#' * 0 = Not at all
#' * 1 = A little bit
#' * 2 = Moderately
#' * 3 = Quite a bit
#' * 4 = Extremely
#'
#' The symptoms correspond to DSM-5 PTSD criteria:
#' * Symptoms 1-5: Criterion B (Intrusion)
#' * Symptoms 6-7: Criterion C (Avoidance)
#' * Symptoms 8-14: Criterion D (Negative alterations in cognitions and mood)
#' * Symptoms 15-20: Criterion E (Alterations in arousal and reactivity)
#'
#' @source Simulated data for demonstration purposes
"simulated_ptsd"
