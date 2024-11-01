##################################################################################
#' Analyze Two-Way Interactions in a Boosted Regression Model for Species and BCR
#'
#' This function extracts and analyzes the two-way interaction spaces (covariate pairs) from
#' a set of Boosted Regression Model (GBM) objects for a given species and Bird Conservation Region (BCR).
#' It calculates the mean interaction effect for each covariate pair and sorts the results by the highest interaction effect.
#'
#' @param data An exported nested `list` (see: `data(boot_pts_sorted_i2)`) where the top level elements are species x bcr x 2-way interaction tuples,
#' and the second-level elements are matrices of the associated average and SD of the bootstrapped prediction spaces
#'
#' @param bcr A `character` or `numeric` vector specifying the Bird Conservation Region(s) to query.
#'
#' @param common_name A `character` vector specifying the species' common name(s) to query.
#'
#' @return A `data.frame` containing the mean and standard deviation of interaction effects for queried covariate pairs,
#' sorted by the highest mean interaction effect. The result includes the BCR, species, and covariate pairs.
#'
#' @importFrom stringr str_split str_split_i
#' @importFrom dplyr mutate arrange desc
#' @importFrom purrr list_rbind
#' @importFrom tibble as_tibble
#' @importFrom gbm plot.gbm
#'
#' @export
#' @rdname interactionsNM
#' @examples
#' interactionsNM(boot_pts_reduced_i2, bcr = c("can12", "can11"), common_name = "Alder Flycatcher")
#'
#'
#' # Assuming `boot_pts_reduced_i2` contains the required GBM data:
#' # Example of querying interactions for Alder Flycatcher in BCRs 11 and 12:
#' # interactionsNM(data = boot_pts_reduced_i2,
#' #                 bcr = c("can12", "can11"),
#' #                 common_name = "Alder Flycatcher")
#'
#' # Plotting the GBM interaction for the lowest and highest mean_y_mean:
#' # plot.gbm(x=b.i, return.grid = FALSE, i.var = c("SCANFITamarack_5x5", "year"), type="response")
#'
##################################################################################

interactionsNM <- function(data = boot_pts_reduced_i2, bcr, common_name){

  # construct the keys for accessing the desired gbm objects
  # note that `paste()` can handle vectors so (e.g.) `bcr` or `common_name` may have length > 1.
  keys <- as.character(outer(bcr, common_name, FUN=paste, sep="."))

  # extract bcr x spp info from boot_pts_sorted_i2
  boot_bcr_spp <-
    paste(stringr::str_split_i(names(boot_pts_reduced_i2), "\\.", 1),
          stringr::str_split_i(names(boot_pts_reduced_i2), "\\.", 2),
          sep=".")

  # subset boot_pts_sorted_i2 to the bcr x spp queried
  queried_bcr_spp <- boot_pts_reduced_i2[which(boot_bcr_spp %in% keys)]

  # for every element of `queried_bcr_spp` (a single 2-way interaction)
  # 1. get mean of the mean +/- sd 2-way interaction space
  # 2. enter as an element into a list
  # 3. flatten list into dataframe
  # 4. sort by highest y

  queried_means_list <- list()
  for (v in 1:length(queried_bcr_spp)){

    # bring bcr, spp, var info along
    info_v <- stringr::str_split(names(queried_bcr_spp)[v], "\\.", simplify=TRUE)

    queried_means_list[[v]] <-
      queried_bcr_spp[[v]] |>
      tibble::as_tibble() |>
      dplyr::mutate(bcr = info_v[1], common_name = info_v[2], var_1 = info_v[3], var_2 = info_v[4])


    # print progress
    cat(paste("\rsearching", v, "of", length(queried_bcr_spp), "2-way interactions"))
    Sys.sleep(0.000001)

  }

  # gather all interaction space means into a single table and sort
  # `round()` is used for tie-breakers where the means are functionally identical but there is large differences in SD
  queried_means <-
    queried_means_list |>
    purrr::list_rbind() |>
    mutate(mean_y_mean = round(mean_y_mean, 3)) |>
    dplyr::arrange(desc(mean_y_mean), mean_y_sd)

  return(queried_means)
}
