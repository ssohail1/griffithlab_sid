#' Return two-letter codes with unique letters and no reversals
#'
#' @description This function returns two-letter codes.
#' For each value, the letters will be unique (e.g., "AB", but never "AA").
#' Also, there will be no reversals (e.g., "AB", but never "BA").
#' This function is used to facilitate randomisation codes for experiments and
#' clinical trials
#'
#' @details
#' Only uppercase letters are used. If desired, the user can delete letters
#' that resemble numerals, and  similar-looking letters,
#' e.g., I, L, O, U, V, and Z (and maybe others).
#' The input x will sorted in ascending order.
#'
#' @param x Set to LETTERS by default ("A", "B", "C", and so on.)
#'
#' @return All possible two-letter codes with no repeats and with no reversals.
#' @export
#'
#' @examples
#'\dontrun{
#' two_letter_codes()
#' two_letter_codes(LETTERS[1:10]
#' two_letter_codes("A", "B", "C", "D", "E", "F")
#' }
two_letter_codes <- function(x = LETTERS) {

  # Only unique values in x will be used
  x <- unique(toupper(x))

  # Sort x
  x <- x[order(x)]

  # Check for bad input
  x_lengths <- vapply(x, nchar, 1)

  if(any(x_lengths > 1)) {
    stop("All elements of x should be one character in length. Try again.")
  }

  if(!all(x %in% LETTERS)) {
    stop("All elements of x should be a letter. Try again.")
  }

  codes_2L <- vector(mode = "character")
  p <- 0 # p for "vector position"

  for (i in seq_len(length(x) - 1)) {

    first_letter <- x[i]

    # Indices of all possible second letters
    second_letters <- x[seq(i + 1, length(x))]

    for(j in seq_along(second_letters)) {

      p <- p + 1

      # do you want to add here 
      # condition of if first letter is not equal to second letter 
      # then paste first and second letter together?

      second_letter <- second_letters[j]

      codes_2L[p] <- paste0(first_letter, second_letter)

    }
  }

  # Return output
  codes_2L

}
