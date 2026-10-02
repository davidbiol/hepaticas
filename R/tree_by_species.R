#' Prunes a phylogenetic tree to include only a specified set of species
#'
#' @param species_vector A character vector containing the names of the species
#'        that should be retained in the new tree. These names must exactly match
#'        tip labels in the input_tree.
#' @param input_tree An object of class 'phylo' representing the phylogenetic tree,
#'   or a character string specifying the file path to a `.nwk`, `.tre`, or `.rda` file.
#'   Defaults to the package dataset `speciesTree`.
#' @param tolerance A number representing the tolerance of character edits for each element of species_vector input. tolerance = 0 means there is a perfect word match. Default is 2 (i.e. 2 character edits is the maximum tolerance).
#' @return A new 'phylo' object containing only the specified species, or NULL
#'         if an error occurs (e.g., no common species found).
#' @export
#' @examples
#' species_list <- c("Herbertus sendtneri", "Micropterygium carinatum", "Lepidozia pinnaticruris", "Bazzania pallidevirens", "Bazzania jamaicensis", "Plagiochila simplex", "Plagiochila revolvens")
#' output_tree <- tree_by_species(species_list)
#' plot(output_tree) #Graph
tree_by_species <- function(species_vector, input_tree = speciesTree, tolerance = 2) {

  # Resolve and Load `input_tree`
  if (is.character(input_tree) && length(input_tree) == 1) {
    if (!file.exists(input_tree)) {
      stop(paste0("Specified file path does not exist: ", input_tree))
    }

    ext <- tolower(tools::file_ext(input_tree))

    if (ext %in% c("nwk", "tre", "tree")) {
      input_tree <- ape::read.tree(input_tree)
    } else if (ext == "rda" || ext == "rdata") {
      env <- new.env()
      loaded_objs <- load(input_tree, envir = env)
      phylo_objs <- sapply(loaded_objs, function(obj) inherits(env[[obj]], "phylo"))

      if (!any(phylo_objs)) {
        stop("The .rda file does not contain an object of class 'phylo'.")
      }
      # Take the first 'phylo' object found in the .rda
      input_tree <- env[[ loaded_objs[phylo_objs][1] ]]
    } else {
      stop("Unsupported file format. Please provide a path to a .nwk, .tre, or .rda file.")
    }
  }

  # Ensure input is now a 'phylo' object
  if (!inherits(input_tree, "phylo")) {
    stop("Input 'input_tree' must be an object of class 'phylo' or a valid file path.")
  }

  # Get all tip labels from the input tree
  input_tree$tip.label <- gsub("_", " ", input_tree$tip.label)
  all_tips <- input_tree$tip.label

  # Standarize names
  species_vector <- stringr::str_to_sentence(species_vector)
  species_vector <- gsub("_", " ", species_vector)

  # Identify mismatches
  validate_species <- function(input_species, valid_options) {

    # Calculate Levenshtein distance (edit distance)
    distances <- utils::adist(input_species, valid_options)

    # Find the index of the closest valid option
    best_match_index <- which.min(distances)
    min_distance <- min(distances)

    # Get the closest valid string
    closest_match <- valid_options[best_match_index]

    # --- Logic for Error/Suggestion ---

    # Condition 1: Perfect Match (No error, continue)
    if (min_distance == 0) {
      return(input_species)
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
          "x" = paste0("Invalid input: \"", input_species, "\"."),
          # The desired suggestion line (using the 'i' structure for info)
          "i" = paste0("Did you mean \"", closest_match, "\"?")
        ),
        class = "species_mismatch_error" # Custom error class
      )

    } else {
      # Condition 3: No Acceptable Match (Throw hard error)
      rlang::abort(
        paste0("Input '", input_species, "' is not a valid species and is too dissimilar from known options."),
        class = "species_mismatch_error"
      )
    }
  }

  # This will trigger the custom error output:
  for (i in seq_len(length(species_vector))){
    tryCatch(
      validate_species(species_vector[i], valid_options = all_tips),
      error = function(e) {
        # This structure is necessary to print the formatted rlang error message
        cat(conditionMessage(e), "\n")
      }
    )
  }

  # Identify species in 'species_to_keep' that are NOT in the input tree
  species_not_found <- setdiff(species_vector, all_tips)
  if (length(species_not_found) > 0) {
    warning(paste("The following species were not found in the input tree and will be ignored:",
                  paste(species_not_found, collapse = ", ")))
  }

  # Identify species to drop: all tips in the input tree MINUS the ones we want to keep
  # We only consider species_to_keep that are actually present in the tree
  valid_species_to_keep <- intersect(species_vector, all_tips)

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
