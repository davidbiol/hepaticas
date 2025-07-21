#' Prunes a phylogenetic tree to include only a specified set of species
#'
#' @param input_tree An object of class 'phylo' representing the input phylogenetic tree.
#' @param species_to_keep A character vector containing the names of the species
#'        that should be retained in the new tree. These names must exactly match
#'        tip labels in the input_tree.
#' @return A new 'phylo' object containing only the specified species, or NULL
#'         if an error occurs (e.g., no common species found).
#' @export
tree_by_species <- function(species, input_tree) {
  data("speciesTree")
  input_tree <- speciesTree
  # Ensure the input is a 'phylo' object
  if (!inherits(input_tree, "phylo")) {
    stop("Input 'input_tree' must be an object of class 'phylo'.")
  }

  # Get all tip labels from the input tree
  all_tips <- input_tree$tip.label

  # Identify species in 'species_to_keep' that are NOT in the input tree
  species_not_found <- setdiff(species, all_tips)
  if (length(species_not_found) > 0) {
    warning(paste("The following species were not found in the input tree and will be ignored:",
                  paste(species_not_found, collapse = ", ")))
  }

  # Identify species to drop: all tips in the input tree MINUS the ones we want to keep
  # We only consider species_to_keep that are actually present in the tree
  valid_species_to_keep <- intersect(species, all_tips)

  if (length(valid_species_to_keep) == 0) {
    message("No valid species to keep were found in the input tree. Returning NULL.")
    return(NULL)
  }

  species_to_drop <- setdiff(all_tips, valid_species_to_keep)

  # If all species are to be kept, return the input tree
  if (length(species_to_drop) == 0) {
    message("All specified species are already present and no tips need to be dropped. Returning input tree.")
    return(input_tree)
  }

  # Use the drop.tip function from the 'ape' package to prune the tree
  # This function removes the specified tips and re-calculates branch lengths and node positions.
  pruned_tree <- tryCatch({
    ape::drop.tip(input_tree, species_to_drop)
  }, error = function(e) {
    message("An error occurred during tree pruning: ", e$message)
    return(NULL)
  })

  return(pruned_tree)
}

#Es importante agregar la función que aproxima el nombre cuando está mal escrito

#new_tree <- prune_tree_by_species(speciesTree, c("Bazzania", "Bazzania", "Calypogeia", "Trichocolea", "Leiomitra", "Mnioloma", "Paracromastigum", "Pseudocephalozia", "Telaranea", "Zoopsidella"))
#plot(new_tree)
