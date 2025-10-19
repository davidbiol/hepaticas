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
