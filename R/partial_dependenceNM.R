##################################################################################
#' Partial Dependence Plot for Species in a BCR Based on Covariate
#'
#' This function generates a partial dependence plot for a given species, BCR (Bird Conservation Region), and
#' covariate, i.e. how the response variable changes over the range of the covariate.
#' The function fits a smooth spline to bootstrap replicates, aggregates predictions,
#' and visualizes the mean response with error bounds.
#'
#' @param data A named list of \code{data.frames}, where each element contains bootstrap replicates for different combinations of BCRs, species, and covariates.
#' The key for each element in the list is a combination of \code{bcr}, \code{common_name}, and \code{covariate} (e.g., \code{"12_BAOR_temp"}).
#'
#' @param bcr A \code{character} specifying the Bird Conservation Region (BCR) for which the partial dependence plot is to be generated.
#'
#' @param common_name A \code{character} specifying the species to generate a partial dependence plot for.
#'
#' @param covariate A \code{character} specifying the covariate (e.g., temperature, precipitation) whose effect is to be visualized in the partial dependence plot.
#'
#' @return A \code{ggplot} showing the partial dependence plot, including the mean response and an error ribbon representing the 95% confidence interval.
#' If the specified combination of BCR, species, and covariate does not exist in the data, the function returns an error.
#'
#' @importFrom rlang sym
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme_minimal
#' @importFrom stats smooth.spline predict quantile
#'
#' @export
#' @examples
#'
#' partial_dependenceNM(boot_pts_sorted, bcr = "can10", common_name = "Alder Flycatcher", covariate = "year")
#'
#'
##################################################################################




partial_dependenceNM <- function(data = boot_pts_sorted, bcr, common_name, covariate) {

  # construct the key for accessing the desired data frame
  key <- paste(bcr, common_name, covariate, sep = "_")

  # check if the key exists in the data
  if (!key %in% names(data)) {
    stop("The specified combination of species, bcr, and covariate does not exist.")
  }

  # combine all the relevant into a single data frame with a column`replicate` with its bootstrap ID
  combined_df <- bind_rows(data[[key]], .id = "replicate")

  # convert the covariate to a symbol for dynamic grouping
  covariate_sym <- rlang::sym(covariate)

  # create a vector of x values (covering the domain of the combined bootstraps) for prediction using `smooth.spline()`
  x_grid <- seq(min(combined_df[[covariate]]), max(combined_df[[covariate]]), length.out = 1000)

  # fit a smoothing function to each bootstrap replicate and predict over the domain of x values
  predictions <-
    lapply(data[[key]], function(df) {
      fit <- smooth.spline(df[[covariate]], df$y)
      predict(fit, x_grid)$y
    })

  # combine predictions into a data frame
  prediction_df <- do.call(cbind, predictions)

  # simplify column names
  colnames(prediction_df) <- paste0("replicate_", seq_along(predictions))
  prediction_df <- as_tibble(prediction_df)
  prediction_df[[covariate]] <- x_grid

  # calculate summary statistics (mean and error bounds) for each x value
  summary_df <-
    prediction_df |>
    pivot_longer(data = _, cols = starts_with("replicate_"), names_to = "replicate", values_to = "predicted_response") |>
    group_by(.data = _, !!covariate_sym) |>
    summarise(
      mean_response = mean(predicted_response),
      lower_bound = quantile(predicted_response, 0.025),
      upper_bound = quantile(predicted_response, 0.975)
    )

  # create a partial dependence plot with error envelope
  ggplot(summary_df, aes(x = !!covariate_sym, y = mean_response)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2, fill = "blue") +
    labs(title = paste("Partial Dependence Plot for", common_name, "in BCR", bcr, "and Covariate", covariate, "across 10 bootstraps"),
         x = covariate, y = "singing males per Ha") +
    theme_minimal()


}
