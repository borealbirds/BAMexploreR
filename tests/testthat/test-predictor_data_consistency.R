# `bam_predictor_importance_v5` is derived from `bam_predictor_boot_v5`
# by averaging over bootstraps. They were previously produced by
# separate runs of the V5 estimation script and drifted apart for 7 of 1385 models
# (one of which had been refit with a different predictor set). These
# tests prevent silent failure.

test_that("bam_predictor_importance_v5 is exactly derivable from bam_predictor_boot_v5", {
  boot <- BAMexploreR:::.get_dataset("bam_predictor_boot_v5")
  imp  <- BAMexploreR:::.get_dataset("bam_predictor_importance_v5")

  derived <-
    boot |>
    dplyr::group_by(spp, bcr, predictor, predictor_class) |>
    dplyr::summarise(mean_rel_inf = mean(rel.inf, na.rm = TRUE),
                     sd_rel_inf   = stats::sd(rel.inf, na.rm = TRUE),
                     n_boots      = sum(rel.inf > 0),
                     .groups      = "drop") |>
    dplyr::select(spp, bcr, predictor, mean_rel_inf, sd_rel_inf, n_boots,
                  predictor_class) |>
    dplyr::arrange(spp, bcr, dplyr::desc(mean_rel_inf)) |>
    as.data.frame()

  expect_equal(derived, imp, tolerance = 1e-12)
})

test_that("relative influence sums to 100 within every model x bootstrap", {
  totals <-
    BAMexploreR:::.get_dataset("bam_predictor_boot_v5") |>
    dplyr::group_by(spp, bcr, boot) |>
    dplyr::summarise(total = sum(rel.inf), .groups = "drop")

  expect_equal(max(abs(totals$total - 100)), 0, tolerance = 1e-10)
})

test_that("every predictor carries a class and coverage matches the importance table", {
  boot <- BAMexploreR:::.get_dataset("bam_predictor_boot_v5")
  imp  <- BAMexploreR:::.get_dataset("bam_predictor_importance_v5")

  expect_equal(sum(is.na(boot$predictor_class)), 0L)
  expect_equal(dplyr::distinct(boot, spp, bcr) |> dplyr::arrange(spp, bcr),
               dplyr::distinct(imp, spp, bcr) |> dplyr::arrange(spp, bcr))
})

test_that(".predictor_class_boot returns shares summing to 1 per model x bootstrap", {
  cb <- BAMexploreR:::.predictor_class_boot("v5")

  expect_named(cb, c("spp", "bcr", "boot", "predictor_class", "share"))

  totals <-
    cb |>
    dplyr::group_by(spp, bcr, boot) |>
    dplyr::summarise(total = sum(share), .groups = "drop")

  expect_equal(max(abs(totals$total - 1)), 0, tolerance = 1e-10)
  # v4 ships no bootstrap-level data, so the caller must fall back
  expect_null(BAMexploreR:::.predictor_class_boot("v4"))
})
