#' National Models 5.0 - covariate interpretation
#'
#' @param covariate_data A data frame with rows as covariates and columns denoting relative influence.
#' @param species The species to be summarized.
#' @param bcr Bird Conservation Regions to be summarized.
#' @param traits Traits for the species, e.g., `avonet` or `acad`.
#' @param groups Grouping variables for summarizing model covariates.
#' @param plot Logical; if `TRUE`, creates a stacked bar plot.
#' @param colours Character vector of hex codes for colours.
#' @param export Logical; if `TRUE`, exports the underlying data.
#'
#' @importFrom rlang sym
#' @importFrom dplyr filter summarise group_by left_join
#' @importFrom ggplot2 ggplot aes geom_bar theme theme_classic element_text
#'
#' @return A stacked bar chart or a dataframe if `export = TRUE`.
#' @export
#' @rdname stackedbarNM
#' @examples
#' stackedbarNM(covariate_data, species = "BAOR", bcr = 12)
stackedbarNM <- function(data = bam_covariate_importance, groups = NULL, species = "all", bcr = "all", traits = NULL, plot = FALSE, colours = NULL){


  # Filter the dataset by species if specified
  user_species <- species

  if (user_species != "all" &
      (all(user_species %in% data$common_name)==TRUE | all(user_species %in% data$sci_name)==TRUE)) {
    bam_covariate_importancel <-
      bam_covariate_importance |>
      dplyr::filter(species %in% user_species)
  }


  # If user specifies built-in dataset, load it via `data()`, otherwise use user-specified dataset
  # if (traits %in% c("avonet", "acad")){
  #   traits <- data(traits)
  # } else {traits <- trait}


  # Check that trait data is the right class
  if (is.null(traits) == FALSE & is.data.frame(traits) == FALSE) {
    print("argument `traits` must be NULL or a `data.frame`")
  }

  # for dplyr::group_by
  group_sym <- rlang::syms(groups)


  # sum covariate importance across for every permutation of group1 and group2
  rel_inf_sum <-
    bam_covariate_importance |>
    group_by(!!!group_sym) |>
    summarise(sum_influence = sum(rel.inf), .groups="keep")


  # sum of covariate importance for each of group1 (all group2 sums are amalgamated into group1 bins)
  group1_sum <-
    rel_inf_sum |>
    group_by(!!group_sym[[1]])  |>
    summarise(sum_group1 = sum(sum_influence), .groups="keep")

  # get the %contribution of group2 covariates to overall covariate importance for a given group1
  proportion_inf <-
    rel_inf_sum |>
    left_join(x = _, group1_sum, by=groups[1]) |>
    mutate(prop = sum_influence/sum_group1)


  barplot <-
    ggplot() +
    geom_bar(aes(x=!!group_sym[[1]], y=prop, fill=!!group_sym[[2]]), data=proportion_inf, stat = "identity") +
    # scale_fill_manual(values = colours) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  print(barplot)
  print(paste("plotting proportion of variable influence by", groups[1], "and", groups[2], sep=" "))

}


