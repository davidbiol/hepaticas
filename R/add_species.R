#' Add Species to Tree and Update Package Data
#'
#' @param species A character vector of species names to add (e.g., c("Bazzania magnifica", "Mnioloma sp. 1"))
#' @param input_nwk_path Path to the input Newick file. Defaults to "newickTree/speciesTree.nwk"
#' @param output_nwk_path Path to the output Newick file. Defaults to "newickTree/speciesTree_proof.nwk"
#' @param output_rda_path Path to the utput Rda file. Defaults to "data/speciesTree_proof.Rda"
#' @return Inserts the tips, write the files, and returns the updated phylo object invisibly.
#' @export
add_species <- function(species,
                                input_nwk_path = "trees/newick/speciesTree_proof.nwk",
                                output_nwk_path = "trees/newick/speciesTree_proof.nwk",
                                output_rda_path = "data/speciesTree_proof.Rda") {
  # 1. Require necessary phylogenetic packages
  if (!requireNamespace("ape", quietly = TRUE)) stop("Package 'ape' is required.")
  if (!requireNamespace("phytools", quietly = TRUE)) stop("Package 'phytools' is required.")

  # 2. Load the existing tree (from Newick to ensure exact string matching)
  if (!file.exists(input_nwk_path)) stop(paste("Newick file not found at:", input_nwk_path))
  tree <- ape::read.tree(file=input_nwk_path)

  # Clean underscore/space formatting just in case
  tree$tip.label <- gsub(" ", "_", tree$tip.label)
  species <- gsub(" ", "_", species)

  # 3. Loop through each new species to add
  for (sp in species) {
    # Skip if species already exists in the tree
    if (sp %in% tree$tip.label) {
      warning(paste("Species", sp, "is already in the tree. Skipping."))
      next
    }

    # Extract the genus (assuming "Genus_species" format)
    genus <- strsplit(sp, "_")[[1]][1]

    # Find all tips in the tree belonging to this genus
    genus_tips <- tree$tip.label[grep(paste0("^", genus, "_"), tree$tip.label)]

    if (length(genus_tips) == 0) {
      warning(paste("No existing species found for genus:", genus, "- Cannot place:", sp))
      next
    }

    # Determine where to bind the new tip
    if (length(genus_tips) == 1) {
      # If only one species exists, bind it to the terminal node (creates a cherry)
      where_node <- which(tree$tip.label == genus_tips)
    } else {
      # If multiple species exist, find their Most Recent Common Ancestor (MRCA) node
      where_node <- ape::getMRCA(tree, genus_tips)
    }

    # Bind the new tip. setting position to 0 puts it at the node (polytomy)
    tree <- phytools::bind.tip(tree, tip.label = sp, where = where_node, position = 0)
  }

  # 4. Export and update files
  # Write updated Newick
  ape::write.tree(tree, file = output_nwk_path)

  tree$tip.label <- gsub("_", " ", tree$tip.label)
  speciesTree_proof <- tree
  # Save as .Rda object named 'speciesTree'
  save(speciesTree_proof, file = output_rda_path, compress = "xz") # xz compression is great for package data
  usethis::use_data(speciesTree_proof, overwrite = TRUE)

  message("Successfully updated speciesTree.nwk and speciesTree.Rda!")
  return(invisible(speciesTree))
}
