#' Returns scores for the 20-item Pelvic Floor Distress Inventory
#' (PFDI) based on a dataframe of input.
#'
#' @description This function returns takes a dataframe, extracts the
#' PFDI-20 items, calculates subscale scores, and returns the
#' scores along with any other requested variables from the input.
#'
#' @details
#' If only a subset of variables are desired to be returned,
#' the column names can be specified in transfer_vars.
#' It should be noted that if any out-of-range or non-numeric
#' (e.g., character) values are detected, they are re-coded to NA.
#'
#' @param input A dataframe containing PFDI items. Other columns may also
#' be present and will be returned by the function (if desired). The items must
#' use the recommended names: pfdi_20_1, pfdi_20_1a, pfdi_20_2, pfdi_20_2a.
#' Factor variables will result in an error.
#'
#' @section Item response coding: Items are coded 0/1 for the No/Yes items and
#' Items "a" are coded 1-4. This coding must be respected.
#'
#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with the
#' PFDI scores.
#'
#' @return A dataframe of output containing PFDI scores.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' score_pfdi(input = some_data)
#' }
score_pfdi <- function(input,
                       transfer_vars = names(input)) {

  # Check for errors in the the arguments
  check_args_score_pfdi(input = input,
                              transfer_vars = transfer_vars)

  pfdi_vnames <-  c("pfdi_20_1", "pfdi_20_1_a",
                    "pfdi_20_2", "pfdi_20_2_a",
                    "pfdi_20_3", "pfdi_20_3_a",
                    "pfdi_20_4", "pfdi_20_4_a",
                    "pfdi_20_5", "pfdi_20_5_a",
                    "pfdi_20_6", "pfdi_20_6_a",
                    "pfdi_20_7", "pfdi_20_7_a",
                    "pfdi_20_8", "pfdi_20_8_a",
                    "pfdi_20_9", "pfdi_20_9_a",
                    "pfdi_20_10", "pfdi_20_10_a",
                    "pfdi_20_11", "pfdi_20_11_a",
                    "pfdi_20_12", "pfdi_20_12_a",
                    "pfdi_20_13", "pfdi_20_13_a",
                    "pfdi_20_14", "pfdi_20_14_a",
                    "pfdi_20_15", "pfdi_20_15_a",
                    "pfdi_20_16", "pfdi_20_16_a",
                    "pfdi_20_17", "pfdi_20_17_a",
                    "pfdi_20_18", "pfdi_20_18_a",
                    "pfdi_20_19", "pfdi_20_19_a",
                    "pfdi_20_20", "pfdi_20_20_a")

  # Extract PFDI items
  pfdi <- input[pfdi_vnames]

  # Stop execution if factors are present
  if (any(apply(pfdi, 2, is.factor))) {
    stop("No factors are allowed as input. Please ensure your items are ",
         "numeric and try again.")
    }

  n <- nrow(pfdi)

  # Coerce character data to numeric
  pfdi <- suppressWarnings(
    vapply(pfdi, as.numeric, numeric(n)))

  pfdi <- as.data.frame(pfdi)

  pfdi_yn <- paste0("pfdi_20_", 1:20)
  pfdi_a <- paste0(pfdi_yn, "_a")

  pfdi_0_4 <-
    mapply(combine_pfdi_items,
           pfdi[pfdi_yn],
           pfdi[pfdi_a],
           USE.NAMES = FALSE)

  popdi6_items <- pfdi_0_4[, 1:6]
  crad8_items <- pfdi_0_4[, 7:14]
  udi6_items <- pfdi_0_4[, 15:20]

  popdi6 <- apply(popdi6_items, 1, function(x) mean(x, na.rm = TRUE)) * 25
  crad8 <- apply(crad8_items, 1, mean, na.rm = TRUE) * 25
  udi6 <- apply(udi6_items, 1, mean, na.rm = TRUE) * 25

  popdi6[is.nan(popdi6)] <- NA
  crad8[is.nan(crad8)] <- NA
  udi6[is.nan(udi6)] <- NA

  pfdi_sum <- popdi6 + crad8 + udi6

  # Return output
  data.frame(input[transfer_vars], popdi6, crad8, udi6, pfdi_sum)

}
