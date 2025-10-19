#' Prunes a phylogenetic tree to include only a specified set of genera
#'
#' @param genus_vector A character vector containing the names of the genera
#'        that should be retained in the new tree. These names must exactly match
#'        tip labels in the input_tree.
#' @param input_tree An object of class 'phylo' representing the input phylogenetic tree.
#' @param tolerance A number representing the tolerance of character edits for each element of genus_vector input. tolerance = 0 means there is a perfect word match. Default is 2 (i.e. 2 character edits is the maximum tolerance).
#' @return A new 'phylo' object containing only the specified genus, or NULL
#'         if an error occurs (e.g., no common genus found).
#' @export
#' @examples
#' genus_list <- c("Bazzania", "Bazzania", "Calypogeia", "Trichocolea", "Leiomitra", "Mnioloma", "Paracromastigum", "Pseudocephalozia", "Telaranea", "Zoopsidella")
#' output_tree <- tree_by_genus(genus_list)
#' plot(output_tree) #Graph
tree_by_genus <- function(genus_vector, input_tree, tolerance = 2) {
  data("genusTree")
  input_tree <- genusTree
  # Ensure the input is a 'phylo' object
  if (!inherits(input_tree, "phylo")) {
    stop("Input 'input_tree' must be an object of class 'phylo'.")
  }

  # Get all tip labels from the input tree
  all_tips <- input_tree$tip.label

  # Identify mismatches
  validate_genus <- function(input_genus, valid_options) {

    # Calculate Levenshtein distance (edit distance)
    distances <- utils::adist(input_genus, valid_options)

    # Find the index of the closest valid option
    best_match_index <- which.min(distances)
    min_distance <- min(distances)

    # Get the closest valid string
    closest_match <- valid_options[best_match_index]

    # --- Logic for Error/Suggestion ---

    # Condition 1: Perfect Match (No error, continue)
    if (min_distance == 0) {
      return(input_genus)
    }

    # Condition 2: Close Match (Throw error with suggestion)
    # Set a reasonable tolerance, e.g., 1 or 2 character edits
    if (min_distance <= tolerance) {

      # We use rlang::abort to construct the exact error message with the hint.
      # The 'class' argument prevents the error message from being simplified
      # and ensures the hint is visible.
      rlang::abort(
        message = c(
          # The main error line (you can make this a placeholder or hide it)
          "x" = paste0("Invalid input: \"", input_genus, "\"."),
          # The desired suggestion line (using the 'i' structure for info)
          "i" = paste0("Did you mean \"", closest_match, "\"?")
        ),
        class = "genus_mismatch_error" # Custom error class
      )

    } else {
      # Condition 3: No Acceptable Match (Throw hard error)
      rlang::abort(
        paste0("Input '", input_genus, "' is not a valid genus and is too dissimilar from known options."),
        class = "genus_mismatch_error"
      )
    }
  }

  # This will trigger the custom error output:
  for (i in seq_len(length(genus_vector))){
    tryCatch(
      validate_genus(genus_vector[i], valid_options = all_tips),
      error = function(e) {
        # This structure is necessary to print the formatted rlang error message
        cat(conditionMessage(e), "\n")
      }
    )
  }

  # Identify genus in 'genus_to_keep' that are NOT in the input tree
  genus_not_found <- setdiff(genus_vector, all_tips)
  if (length(genus_not_found) > 0) {
    warning(paste("The following genus were not found in the input tree and will be ignored:",
                  paste(genus_not_found, collapse = ", ")))
  }

  # Identify genus to drop: all tips in the input tree MINUS the ones we want to keep
  # We only consider genus_to_keep that are actually present in the tree
  valid_genus_to_keep <- intersect(genus_vector, all_tips)

  if (length(valid_genus_to_keep) == 0) {
    message("No valid genus to keep were found in the input tree. Returning NULL.")
    return(NULL)
  }

  genus_to_drop <- setdiff(all_tips, valid_genus_to_keep)

  # If all genus are to be kept, return the input tree
  if (length(genus_to_drop) == 0) {
    message("All specified genus are already present and no tips need to be dropped. Returning input tree.")
    return(input_tree)
  }

  # Use the drop.tip function from the 'ape' package to prune the tree
  # This function removes the specified tips and re-calculates branch lengths and node positions.
  pruned_tree <- tryCatch({
    ape::drop.tip(input_tree, genus_to_drop)
  }, error = function(e) {
    message("An error occurred during tree pruning: ", e$message)
    return(NULL)
  })

  return(pruned_tree)
}
