#' Prunes a phylogenetic tree to include only a specified set of genera
#'
#' @param genus_vector A character vector containing the names of the genera
#'        that should be retained in the new tree. These names must exactly match
#'        tip labels in the input_tree.
#' @param input_tree An object of class 'phylo' representing the input phylogenetic tree.
#' @return A new 'phylo' object containing only the specified genus, or NULL
#'         if an error occurs (e.g., no common genus found).
#' @export
tree_by_genus <- function(genus_vector, input_tree) {
  data("genusTree")
  input_tree <- genusTree
  # Ensure the input is a 'phylo' object
  if (!inherits(input_tree, "phylo")) {
    stop("Input 'input_tree' must be an object of class 'phylo'.")
  }

  # Get all tip labels from the input tree
  all_tips <- input_tree$tip.label

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
