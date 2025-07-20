# Install and load the 'ape' package if you haven't already
# install.packages("ape")
library(ape)

#' Prunes a phylogenetic tree to include only a specified set of species.
#'
#' @param original_tree An object of class 'phylo' representing the original phylogenetic tree.
#' @param species_to_keep A character vector containing the names of the species
#'        that should be retained in the new tree. These names must exactly match
#'        tip labels in the original_tree.
#' @return A new 'phylo' object containing only the specified species, or NULL
#'         if an error occurs (e.g., no common species found).
#' @examples
#' # 1. Create a sample phylogenetic tree (for demonstration purposes)
#' # This is a simple tree with 5 tips.
#' tree_string <- "((speciesA:0.1,speciesB:0.2):0.3,(speciesC:0.4,(speciesD:0.5,speciesE:0.6):0.7):0.8);"
#' original_tree_example <- read.tree(text = tree_string)
#'
#' # Plot the original tree to see its structure
#' plot(original_tree_example, main = "Original Tree")
#'
#' # 2. Define the species you want to keep
#' my_species <- c("speciesA", "speciesD", "speciesE")
#'
#' # 3. Prune the tree
#' pruned_tree <- prune_tree_by_species(original_tree_example, my_species)
#'
#' # 4. Plot the pruned tree if it was successfully created
#' if (!is.null(pruned_tree)) {
#'   plot(pruned_tree, main = "Pruned Tree with Selected Species")
#' } else {
#'   message("Pruning failed. Check if species names are correct and present in the tree.")
#' }
#'
#' # Example with species not in the tree
#' my_species_invalid <- c("speciesA", "nonExistentSpecies")
#' pruned_tree_invalid <- prune_tree_by_species(original_tree_example, my_species_invalid)
#' if (is.null(pruned_tree_invalid)) {
#'   message("Correctly handled invalid species input.")
#' }
prune_tree_by_species <- function(original_tree, species_to_keep) {
  # Ensure the input is a 'phylo' object
  if (!inherits(original_tree, "phylo")) {
    stop("Input 'original_tree' must be an object of class 'phylo'.")
  }

  # Get all tip labels from the original tree
  all_tips <- original_tree$tip.label

  # Identify species in 'species_to_keep' that are NOT in the original tree
  species_not_found <- setdiff(species_to_keep, all_tips)
  if (length(species_not_found) > 0) {
    warning(paste("The following species were not found in the original tree and will be ignored:",
                  paste(species_not_found, collapse = ", ")))
  }

  # Identify species to drop: all tips in the original tree MINUS the ones we want to keep
  # We only consider species_to_keep that are actually present in the tree
  valid_species_to_keep <- intersect(species_to_keep, all_tips)

  if (length(valid_species_to_keep) == 0) {
    message("No valid species to keep were found in the original tree. Returning NULL.")
    return(NULL)
  }

  species_to_drop <- setdiff(all_tips, valid_species_to_keep)

  # If all species are to be kept, return the original tree
  if (length(species_to_drop) == 0) {
    message("All specified species are already present and no tips need to be dropped. Returning original tree.")
    return(original_tree)
  }

  # Use the drop.tip function from the 'ape' package to prune the tree
  # This function removes the specified tips and re-calculates branch lengths and node positions.
  pruned_tree <- tryCatch({
    drop.tip(original_tree, species_to_drop)
  }, error = function(e) {
    message("An error occurred during tree pruning: ", e$message)
    return(NULL)
  })

  return(pruned_tree)
}

#Es importante agregar la función que aproxima el nombre cuando está mal escrito

#new_tree <- prune_tree_by_species(genusTree, c("Bazzania", "Bazzania", "Calypogeia", "Trichocolea", "Leiomitra", "Mnioloma", "Paracromastigum", "Pseudocephalozia", "Telaranea", "Zoopsidella"))
#plot(new_tree)
