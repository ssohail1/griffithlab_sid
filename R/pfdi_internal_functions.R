check_args_score_pfdi <- function(input, transfer_vars) {

  if (!is.data.frame(input)) {
    stop("Your input must be a dataframe. Please try again",
         call. = FALSE)
  }

  returned_vars <- c(
    "popdi6",
    "crad8",
    "udi6",
    "pfdi_sum")

  if (any(returned_vars %in% names(input))) {
    stop("Variable names resulting from PFDI scoring are ",
         "already found in the names of your input. ",
         "Please rename or remove these variables from your input ",
         "and try again.\n\n",
         "The offending variables found in your input were:\n\n",
         paste(returned_vars[returned_vars %in% names(input)], collapse = " "),
         call. = FALSE)
  }

  if (ncol(input) < 40) {
    stop("\nYour input has too few columns to score the PFDI.\n",
         "Please re-check your input and try again.",
         call. = FALSE)
  }

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

  if (!all(pfdi_vnames %in% names(input))) {

    pfdi_names_not_found <-
      pfdi_vnames[which(!pfdi_vnames %in% names(input))]

    stop("\nNot all of the names of the PFDI items were ",
         "found in the input.\n\n",
         "The items of the PFDI should be labelled:\n",
         paste(pfdi_vnames, collapse = " "),
         "\n\nThe following PFDI items were not found in the input:\n",
         paste(pfdi_vnames, collapse = " "), # enter pfdi_names_not_found
         "\n\nPlease try again.",
         call. = FALSE)
  }

  if (!all(transfer_vars %in% names(input))) {
    stop("\n\nWe can only return the scoring results of ",
         "the PFDI and ",
         "variables found in the input.\n",
         "Please fix the variables listed in transfer_vars and try again.",
         "For transfer_vars, ",
         "choose only variable names that are found in your input.",
         call. = FALSE)
  }

  invisible(NULL)

}

combine_pfdi_items <- function(item, item_a) {

  if (length(item) != length(item_a)) {
    stop("Item lengths must be equal for Y/N and 1-4 items.")
  }

  item_0_4 <- rep(NA, length = length(item))

  for (i in 1:length(item)) {

    # for each item i in item if i is 0 (No) then add 0 to that item i in item_0_4
    # if it is 1 (Yes) then add the ith item from item_a to the ith item in item_0_4
    # otherwise NA 
    if (is.na(item[i])) {
      item_0_4[i] <- NA
    } else if (item[i] == 0) {
      item_0_4[i] <- 0
    } else if (item[i] == 1) {
      item_0_4[i] <- item_a[i]
    } else {
      item_0_4[i] <- NA
    }

  }

  item_0_4[!item_0_4 %in% 0:4] <- NA

  return(item_0_4)

}
