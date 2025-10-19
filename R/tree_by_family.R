#' Prunes a phylogenetic tree to include only a specified set of families
#'
#' @param family_vector A character vector containing the names of the families
#'        that should be retained in the new tree. These names must exactly match
#'        tip labels in the input_tree.
#' @param input_tree An object of class 'phylo' representing the input phylogenetic tree.
#' @return A new 'phylo' object containing only the specified family, or NULL
#'         if an error occurs (e.g., no common family found).
#' @export
#' @examples
#' family_list <- c("PLAGIOCHILACEAE", "RADULACEAE", "METZGERIACEAE", "MARCHANTIACEAE", "AYTONIACEAE", "LEPICOLEACEAE", "JUNGERMANNIACEAE")
#' output_tree <- tree_by_family(family_list)
#' plot(output_tree) #Graph
tree_by_family <- function(family_vector, input_tree) {
  data("familyTree")
  input_tree <- familyTree
  # Ensure the input is a 'phylo' object
  if (!inherits(input_tree, "phylo")) {
    stop("Input 'input_tree' must be an object of class 'phylo'.")
  }

  # Get all tip labels from the input tree
  all_tips <- input_tree$tip.label

  # Identify mismatches
  validate_family <- function(input_family, valid_options) {

    # Calculate Levenshtein distance (edit distance)
    distances <- utils::adist(input_family, valid_options)

    # Find the index of the closest valid option
    best_match_index <- which.min(distances)
    min_distance <- min(distances)

    # Get the closest valid string
    closest_match <- valid_options[best_match_index]

    # --- Logic for Error/Suggestion ---

    # Condition 1: Perfect Match (No error, continue)
    if (min_distance == 0) {
      return(input_family)
    }

    # Condition 2: Close Match (Throw error with suggestion)
    # Set a reasonable tolerance, e.g., 1 or 2 character edits
    if (min_distance <= 2) {

      # We use rlang::abort to construct the exact error message with the hint.
      # The 'class' argument prevents the error message from being simplified
      # and ensures the hint is visible.
      rlang::abort(
        message = c(
          # The main error line (you can make this a placeholder or hide it)
          "x" = paste0("Invalid input: \"", input_family, "\"."),
          # The desired suggestion line (using the 'i' structure for info)
          "i" = paste0("Did you mean \"", closest_match, "\"?")
        ),
        class = "family_mismatch_error" # Custom error class
      )

    } else {
      # Condition 3: No Acceptable Match (Throw hard error)
      rlang::abort(
        paste0("Input '", input_family, "' is not a valid family and is too dissimilar from known options."),
        class = "family_mismatch_error"
      )
    }
  }

  # This will trigger the custom error output:
  for (i in seq_len(length(family_vector))){
    tryCatch(
      validate_family(family_vector[i], valid_options = all_tips),
      error = function(e) {
        # This structure is necessary to print the formatted rlang error message
        cat(conditionMessage(e), "\n")
      }
    )
  }
  # Identify family in 'family_to_keep' that are NOT in the input tree
  family_not_found <- setdiff(family_vector, all_tips)
  if (length(family_not_found) > 0) {
    warning(paste("The following family were not found in the input tree and will be ignored:",
                  paste(family_not_found, collapse = ", ")))
  }

  # Identify family to drop: all tips in the input tree MINUS the ones we want to keep
  # We only consider family_to_keep that are actually present in the tree
  valid_family_to_keep <- intersect(family_vector, all_tips)

  if (length(valid_family_to_keep) == 0) {
    message("No valid family to keep were found in the input tree. Returning NULL.")
    return(NULL)
  }

  family_to_drop <- setdiff(all_tips, valid_family_to_keep)

  # If all family are to be kept, return the input tree
  if (length(family_to_drop) == 0) {
    message("All specified family are already present and no tips need to be dropped. Returning input tree.")
    return(input_tree)
  }

  # Use the drop.tip function from the 'ape' package to prune the tree
  # This function removes the specified tips and re-calculates branch lengths and node positions.
  pruned_tree <- tryCatch({
    ape::drop.tip(input_tree, family_to_drop)
  }, error = function(e) {
    message("An error occurred during tree pruning: ", e$message)
    return(NULL)
  })

  return(pruned_tree)
}
