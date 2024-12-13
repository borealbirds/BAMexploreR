boxplotNM_v4 <- function(species = "all", bcr = "all", group = NULL, plot = FALSE, colours = NULL){

  # load bam_covariate_importance_v4.rda from data folder
  data <- data("bam_covariate_importance_v4")
  
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
  
  
  
  
  # define BCRs based on user inputs
  ifelse(bcr == "all", bcr_to_filter <- unique(data$bcr), bcr_to_filter <- bcr)

  # for dplyr::group_by
  group_sym <- rlang::syms(c(group, "var_class", "boot"))

  # need to be able to specify what BCRs (or species or bird group, etc) to plot by
  # sum rel. influence of the grouped variable (e.g. species) per `var_class` and `boot` replicate
  rel_inf_sum <-
    bam_covariate_importance |>
    group_by(!!!group_sym) |>
    filter(bcr %in% bcr_to_filter) |>
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
