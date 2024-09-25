##################################################################################
#' Side-by-Side Boxplots Displaying Bootstrap Variation by Variable Class
#'
#' This function generates side-by-side boxplots that show bootstrap variation of covariate importance by variable class,
#' with options to group by BCR (Bird Conservation Region), species, and other factors.
#'
#' @param data A `data.frame` containing covariate importance values,
#' with rows as covariates and columns denoting the relative influence for a given bootstrap replicate by species by BCR permutation.
#' Defaults to `bam_covariate_importance`.
#'
#' @param group A `character` specifying the grouping variable(s) for summarizing the covariate importance (e.g., species, BCR, etc.).
#'
#' @param species A `character` specifying the species to filter by. The default is `"all"`, which includes all species in the dataset.
#'
#' @param bcr A `character` specifying the Bird Conservation Regions (BCRs) to filter by. The default is `"all"`, which includes all BCRs in the dataset.
#'
#' @param traits A `data.frame` containing species traits to include in the analysis (optional).
#' If `NULL`, traits are not included in the plot.
#'
#' @param plot A `logical` indicating whether to plot the results (`TRUE`) or return the processed data (`FALSE`).
#' Defaults to `FALSE`.
#'
#' @param colours A `character` vector of hex codes for the colours to use in the boxplot (optional).
#' If `NULL`, default colours are used.
#'
#' @return A ggplot object showing boxplots of relative covariate importance by variable class, grouped by the selected factors.
#' If `plot = FALSE`, the processed data is returned instead of the plot.
#'
#' @importFrom dplyr group_by filter summarise left_join
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point labs theme_classic element_text
#'
#' @export
#' @examples
#' # Example usage with default data:
#' # bamexplorer_boxplots(data = bam_covariate_importance, group = "species", species = "all", bcr = "all")
#'
#' # Example with filtering by BCR and custom colours:
#' # bamexplorer_boxplots(data = bam_covariate_importance, group = "species", bcr = c("BCR12", "BCR13"),
#' #                      colours = c("#1f78b4", "#33a02c"))
##################################################################################



boxplotNM <- function(data = bam_covariate_importance, group = NULL, species = "all", bcr = "all", traits = NULL, plot = FALSE, colours = NULL){

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


  ggplot(proportion_inf, aes(x = var_class, y = prop, fill = !!group_sym[[1]])) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha=0.05) +
    geom_point(aes(colour=factor(!!group_sym[[1]])),  position = position_dodge(width = 0.75), alpha=0.7, size=2.5) +
    labs(x = "Variable Class", y = "Relative Importance (%)",
         title = paste("Covariate importance by", group, sep=" ")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
