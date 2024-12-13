##################################################################################
#' Side-by-Side Boxplots Displaying Bootstrap Variation by Variable Class
#'
#' This function generates side-by-side boxplots that show bootstrap variation of covariate importance by variable class,
#' with options to group by BCR (Bird Conservation Region), species, and other factors.
#'
#' @param data Defaults to BAM's covariate importance data but can be replaced with any `data.frame` containing covariate importance values,
#' with covariates as rows and a column each for `bcr`, `species`, `var_class`, `boot`, and `rel.inf`. If user data has no bootstrap replicates, 
#' create a `boot` column and fill in the same number (e.g. `1`) for all rows. Leave this argument as `NULL` unless using custom data.
#'
#' @param version A `character`. Defaults to `"v5"`. `"v4"` is also possible but not fully supported for all functions in the first release of this package.
#' If using custom data in the `data` argument, this argument will be ignored.
#' 
#' @param species A `character` specifying the species to filter by. The default is `"all"`, which includes all species in the dataset.
#'
#' @param bcr A `character` specifying the Bird Conservation Regions (BCRs) to filter by. The default is `"all"`, which includes all BCRs in the dataset.
#'
#' @param group A `character` specifying the grouping variable(s) for summarizing the covariate importance (e.g., species, BCR, etc.).
#' 
#' @param plot A `logical` indicating whether to plot the results (`TRUE`) or return the processed data (`FALSE`).
#' 
#' @param colours A `character` vector of hex codes for the colours to use in the boxplot (optional).
#' If `NULL`, default colours are used.
#'
#' @return A ggplot object displaying boxplots of relative covariate importance by variable class, grouped by the `group` argument.
#' If `plot = FALSE`, the processed data is returned instead of the plot.
#'
#' @importFrom dplyr group_by filter summarise left_join mutate
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point labs theme theme_classic element_text
#'
#' @export
#' @rdname boxplotNM
#' @examples
#'
#'
#' # Example of plotting covariate importance for Alder Flycatcher across all BCRs.
#' # boxplotNM(group = "species", species = "ALFL", bcr = "all")
#'
#' # Example of plotting covariate importance for two species in two BCRs, using custom colours:
#' # boxplotNM(group = "species", species = c("ALFL", "CAWA"), 
#' bcr = c("BCR12", "BCR13"),  colours = c("#1f78b4", "#33a02c"))
#' 
#' 
#' 
##################################################################################

boxplotNM <- function(data = NULL, version = "v5", species = "all", bcr = "all", group = NULL, plot = FALSE, colours = NULL) {
  
  # validate data if provided
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }
  
  # Validate version
  if (!version %in% c("v4", "v5") && data == NULL) {
    stop("Invalid `version`. Use 'v4' or 'v5'.")
  }
  
  
  # dispatch appropriate version
  if (!is.null(data)) {
    boxplotNM_custom(data, species = species, bcr = bcr, group = group, plot = plot, colours = colours)
  } else if (version == "v4") {
    boxplotNM_v4(species = species, bcr = bcr, group = group, plot = plot, colours = colours)
  } else if (version == "v5") {
    boxplotNM_v5(species = species, bcr = bcr, group = group, plot = plot, colours = colours)
  }
}
  




