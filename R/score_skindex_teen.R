#' Scores the Skindex-Teen questionnaire using only the 21 scored items
#' from the Smidt et al. paper (cf., Table 1 of doi:10.1001/archdermatol.2010.161)
#'
#' @description Calculates a total score of the 21 Skindex-Teen items
#' (i.e., skindex_teen_1 through skindex_teen_21), as well as two subscales
#' for Physical Symptoms (ps, 5 items) and Psychosocial Functioning (pf, 16 items).
#' If a vector is supplied as input, it will be converted to a one-row,
#' 21-column dataframe. Items are assumed to be in the same order as in Table 1
#' of Smidt et al. but with item 17 of 22 removed.
#'
#' @param items A matrix (or an object coercible to a matrix) that contains
#' the items of the Skindex-teen, with each item
#' represented as a number, 0, 1, 2, 3, or 4. Anything not a 0, 1, 2, 3, or 4 will
#' silently be converted to NA.
#'
#' @param min_prop The minimum proportion of items needed to be non-missing
#' in order for a score to be given. If the observed proportion of non-missing items is
#' less than min_prop, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. With prorating, the score
#' is calculated as (# of scale items * mean item response) The default for this
#' algorithm is min_prop = 1.0.
#'
#' @param skindex_teen_names If left NULL, by default names in the output
#'  will be "skindex_teen_total", "skindex_teen_ps", and "skindex_teen_pf"
#'
#' @return Scores for the Skindex-Teen Total,
#' Physical Symptoms (ps) and Psychosocial Functioning (pf).
#' @export
#'
#' @examples
#'\dontrun{
#' score_skindex_teen(data[skindex_teen_items])
#' }

score_skindex_teen <- function(items,
                      min_prop = 1.0,
                      skindex_teen_names = NULL) {

  if (is.vector(items)) {

    # Convert items to 1 x 21 dataframe
    items <- as.data.frame(t(items))

    return(score_skindex_teen(items,
                              min_prop = min_prop,
                              skindex_teen_names = skindex_teen_names))
    }

  item_range <- 0:4L

  n_items <- 21L

  # Check for input errors
  if (ncol(items) != n_items) {

    stop("The Skindex-Teen has ",
         n_items,
         "items, so there should be ",
         n_items, " columns in items.",
         "\nPlease try again.")

  }

  if (min_prop < 0 || min_prop > 1) {

    stop("min_prop must be between 0-1. \nPlease try again.")

    }

  if(!is.null(skindex_teen_names) && length(skindex_teen_names) != 3) {

    stop("\nIf you wish to use user-supplied names, please supply a charachter vector with the three names.",
         "\nFor example, c(\"total\", \"ps\", \"pf\")")

    }

  # Done checking for errors in input

  items <- as.matrix(items)

  items[which(!items %in% item_range,
                  arr.ind = TRUE)] <- NA

  if (all(is.na(items))) {

    message("All items are missing.\n")
    message("Check your input.\n")
  } else if (any(is.na(items))) {

    message("Some items are missing in Skindex-Teen items.\n")

  }

  if (min_prop < 1 && any(is.na(items))) {

    message("Scoring will use prorating if some items are missing.\n")
    message("If you do not want to prorate scores, set min_prop to 1.0")

    }

  items <- as.data.frame(items, drop = FALSE)

  # Note - There is one item from the 22-item prototype in Smidt et al.
  # that is not used in the scoring. Only the 21 items are considered here.

  # Item column indices
  skindex_teen_total <- 1:21

  # Skindex Teen Physical Symptoms (ps)
  # Item column indices
  skindex_teen_ps <- c(1, 2, 7, 10, 18)

  # Skindex Teen Psychosocial Functioning (pf)
  # Item column names
  skindex_teen_pf <- c(3, 4, 5, 6, 8, 9, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21)

  total <- score_surveys(items[skindex_teen_total], ceiling(min_prop * n_items))
  ps <- score_surveys(items[skindex_teen_ps], ceiling(min_prop * 5))
  pf <- score_surveys(items[skindex_teen_pf], ceiling(min_prop * 16))

  skindex_teen <- data.frame(skindex_teen_total = total,
                             skindex_teen_ps = ps,
                             skindex_teen_pf = pf)

  if(!is.null(skindex_teen_names)) {

    colnames(skindex_teen) <- skindex_teen_names

    }

  return(skindex_teen)

}
