##################################################################################
#' Side-by-Side Boxplots Displaying Bootstrap Variation by Variable Class
#'
#' This function generates side-by-side boxplots that show bootstrap variation of covariate importance by variable class,
#' with options to group by BCR (Bird Conservation Region), species, and other factors.
#'
#' @param data Defaults to BAM's `bam_covariate_importance` but can be replaced with any `data.frame` containing covariate importance values,
#' with covariates as rows and a column each for `bcr`, `species`, `var_class`, `boot`, and `rel.inf`. If user data has no bootstrap replicates, 
#' create a `boot` column and fill in the same number (e.g. `1`) for all rows. 
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
#' # boxplotNM(data = bam_covariate_importance, group = "species", species = "ALFL", bcr = "all")
#'
#' # Example of plotting covariate importance for two species in two BCRs, using custom colours:
#' # boxplotNM(data = bam_covariate_importance, group = "species", species = c("ALFL", "CAWA"), 
#' bcr = c("BCR12", "BCR13"),  colours = c("#1f78b4", "#33a02c"))
#' 
#' 
#' 
##################################################################################



boxplotNM <- function(data = bam_covariate_importance, species = "all", bcr = "all", group = NULL, plot = FALSE, colours = NULL){

  # check if input data is correct `class()`
  if (!is.data.frame(data)) {
    stop("`data` must be a `data.frame` or `tibble`. see: `?boxplotNM`")
  }
  
  # check if all necessary variables are present
  missing_columns <- setdiff("bcr", "species", "var_class", "boot", "rel.inf", colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("`data` is missing the following required column(s):", paste(missing_columns, collapse = ", ")))
  }
  
  # check if user specified species are in `data`
  if (!all(species %in% unique(data$species)) && species != "all") {
    stop(paste("The following species are not in `data`:",
               paste(setdiff(species, unique(data$species)), collapse = ", ")))
  }
  
  # check if user specified BCRs are in `data`
  if (!all(bcr %in% unique(data$bcr)) && bcr != "all") {
    stop(paste("The following BCR(s) are not in `data`:",
               paste(setdiff(bcr, unique(data$bcr)), collapse = ", ")))
  }
  
  # check if user specified `group` is in `data`
  if (is.null(group) || !group %in% colnames(data)) {
    stop("Please specify a valid `group` column that exists in the data.")
  }
  
  # check if user specified `colours` match the number of levels in `group`. 
  if (!is.null(colours)) {
    n_groups <- length(unique(data[[group]]))
    if (length(colours) != n_groups) {
      stop(paste("The length of `colours` does not match the number of levels in `group` 
                 (", n_groups, "). Provide a colour for each level."))
    }
  }
  
  # check for NAs in `data`
  missing_data <- data |> filter(is.na(bcr) | is.na(species) | is.na(var_class) | is.na(boot) | is.na(rel.inf))
  if (nrow(missing_data) > 0) {
    warning("`data` contains missing values in one or more required columns. These rows will be excluded.")
  }
  
  ifelse(bcr == "all", bcr_to_filter <- unique(bam_covariate_importance$bcr), bcr_to_filter <- bcr)

  # for dplyr::group_by
  group_sym <- rlang::syms(c(group, "var_class", "boot"))

  # need to be able to specify what BCRs (or species or bird group, etc) to plot by
  # sum rel. influence of the grouped variable (e.g. species) per `var_class` and `boot` replicate
  rel_inf_sum <-
    bam_covariate_importance |>
    group_by(!!!group_sym) |>
    dplyr::filter(bcr %in% bcr_to_filter) |>
    summarise(sum_influence = sum(rel.inf), .groups="keep")


  # sum of covariate importance for each of group1 (all var_class sums are amalgamated into group1 bins)
  group1_sum <-
    rel_inf_sum |>
    group_by(!!group_sym[[1]], !!group_sym[[2]])  |>
    summarise(sum_group1 = sum(sum_influence), .groups="keep")


  proportion_inf <-
    rel_inf_sum |>
    left_join(x = _, group1_sum, by=c(group, "var_class")) |>
    mutate(prop = sum_influence/sum_group1)

  
  if (plot) {
    
    ggplot(proportion_inf, aes(x = var_class, y = prop, fill = !!group_sym[[1]])) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.05) +
      geom_point(aes(colour = factor(!!group_sym[[1]])),
                 position = position_dodge(width = 0.75), alpha = 0.7, size = 2.5) +
      labs(x = "Variable Class", y = "Relative Importance (%)",
           title = paste("Covariate importance by", group, sep = " ")) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    
    return(proportion_inf)
    
  }
}
