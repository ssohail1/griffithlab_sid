#' Scores the System Usability Scale (SUS; all 10 items)
#'
#' @description Scores the 10-item System Usability Scale (SUS)
#' The ten items are 1, 2, 3, 4, 5 coded in the following way:\cr
#' 1 = Strongly Disagree\cr
#' 2 = Disagree\cr
#' 3 = Neutral\cr
#' 4 = Agree\cr
#' 5 = Strongly Agree\cr
#'
#' @details
#' In some versions of the SUS, only 1 (Strongly Disagree) and 5 (Strongly Agree) are labelled.
#'
#' @param sus_items A matrix (or an object coercible to a matrix) that contains
#' the items of the System Usability Scale (SUS), with each item
#' represented as a number, 1, 2, 3, 4, or 5. If sus_items is a single vector,
#' it will be transposed to a single-row matrix.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. With prorating the score
#' is (10 * mean item response) The default for the SUS is min_num_items = 10.
#'
#' @return Scores for the SUS.
#' @export
#'
#' @examples
#'\dontrun{
#' sus_items <- paste0("sus", 1:10)
#' score_sus(some_data[sus_items])
#' }

score_sus <- function(sus_items,
                      min_num_items = 10) {

  # If a single vector is supplied, convert it to a data frame
  if(is.vector(sus_items) & length(sus_items) == 10) {
    return(score_sus(t(sus_items),
                     min_num_items = min_num_items))
  }

  sus_range <- 1:5L

  n_sus_items <- 10

  if(ncol(sus_items) != n_sus_items) {
    stop("The SUS has",
         n_sus_items,
         "items, so there should be",
         n_sus_items, "columns in sus_items.")
  }

  if(min_num_items > n_sus_items) {
    stop("The SUS has",
         n_sus_items,
         "items, so min_num_items must be",
         n_sus_items,
         "or smaller.")
  }

  if(min_num_items < 1) {
    stop("min_num_items must be greater than 0.")
  }

  sus_items <- as.matrix(sus_items)

  sus_items[which(!sus_items %in% sus_range,
                  arr.ind = TRUE)] <- NA

  if(all(is.na(sus_items))) {
    message("All items are missing in sus_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(sus_items))) {
    message("Some items are missing in sus_items.\n")
  }

  if(min_num_items < n_sus_items && !all(is.na(sus_items))) {
    message("Scoring will use prorating if some items are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_sus_items))
  }

  # For items 1,3,5,7,and 9 the score contribution is the scale position minus 1.
  # For items 2,4,6,8 and 10, the contribution is 5 minus the scale position.
  # Multiply the sum of the scores by 2.5 to obtain the overall value of SU.

  recoded_sus <- matrix(nrow = nrow(sus_items),
                        ncol = ncol(sus_items))

  recoded_sus[, c(1, 3, 5, 7, 9)] <- sus_items[, c(1, 3, 5, 7, 9)] - 1
  recoded_sus[, c(2, 4, 6, 8, 10)] <- 5 - sus_items[, c(2, 4, 6, 8, 10)]

  recoded_sus <- as.data.frame(recoded_sus)

  # Return output
  2.5 * score_surveys(recoded_sus, min_num_items)

}
