
# Input
#
test_that("Check if 'data' argument exists", {
  expect_error(
    relation_hm_gt(),
    "The parameter `data` are required."
  )
})

#
test_that("Test if input data is a data.frame", {
  expect_error(
    relation_hm_gt(data = c(1, 2, 3)),
    "The `data` parameter must be a `data.frame`."
  )
  expect_error(
    relation_hm_gt(data = "test"),
    "The `data` parameter must be a `data.frame`."
  )
})

#
soil_refle_test <- soil_refle[,-1]

test_that("Test if 'wavelength' column does not exist", {
  expect_error(
    relation_hm_gt(data = soil_refle_test),
    "The `data` does not contain the `wavelength` column."
  )
})

#
soil_refle_test <- soil_refle
soil_refle_test$WAVElength_nm2 <- soil_refle$wavelength_nm

relation_test <- try(
  relation_hm_gt(data = soil_refle_test),
  silent = TRUE)

test_that("Test if 'wavelength' column does not exist", {
  expect_false(inherits(relation_test, "try-error"))
})

#
soil_refle_test <- soil_refle
soil_refle_test$a1[20] <- NA

relation_test <- try(
  relation_hm_gt(data = soil_refle_test),
  silent = TRUE)

test_that("Test for missing values in the data", {

  expect_false(inherits(relation_test, "try-error"))

})

# output
#
test_that("Return is a data.frame", {
  expect_s3_class(
    relation_hm_gt(data = soil_refle),
    "data.frame"
  )
})

#
soil_refle_test <- soil_refle[, 1:5]

test_that("Check if output has the correct structure", {
  expect_equal(nrow(relation_hm_gt(soil_refle_test)), 4)
  expect_equal(ncol(relation_hm_gt(soil_refle_test)), 8)
  expect_equal(
    names(relation_hm_gt(soil_refle_test)),
    c(
      "samples", "min_gt", "max_gt",
      "min_hm", "max_hm", "range_gt",
      "range_hm", "relation_hm_gt"
    )
  )
})

#
test_that("Test if 'wavelength' column is absent in the output",{
  expect_false("wavelength" %in% colnames(relation_hm_gt(soil_refle)))
})

#
relation_test <- try(
  relation_hm_gt(data = soil_refle[,1:3],
                 plot = FALSE,
                 points_smoothing = 0.6,
                 pv_tolerance = c(5,3,9,20),
                 hm_gt_limits = list(hm = c(515,785),
                                     gt = c(390,480)),
                 name_wave = "WAVE"),
  silent = TRUE)

test_that("Test all arguments", {

  expect_false(inherits(relation_test, "try-error"))

})

#
test_that("relation_hm_gt() generates a plot when plot = TRUE", {

  temp_file <- tempfile(fileext = ".png")

  png(temp_file)

  relation_hm_gt(data = soil_refle, plot = TRUE)
  dev.off()

  expect_true(file.exists(temp_file))

  unlink(temp_file)
})

# Execution
test_that("The function executes in less than 0.5 second.", {
  expect_lt(system.time(relation_hm_gt(data = soil_refle))[3], 0.5)
})

#
# plot
# library(vdiffr)
#
# test_that("relation_hm_gt() produces a consistent plot", {
#   vdiffr::expect_doppelganger("relation_hm_gt plot", function() {
#     relation_hm_gt(data = soil_refle, plot = TRUE)
#   })
# })
