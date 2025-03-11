
# Input
#
test_that("Check if 'data' argument exists", {
  expect_error(
    soil_color(),
    "The parameter `data` are required."
  )
})

#
test_that("Test if input data is a data.frame", {
  expect_error(
    soil_color(data = c(1, 2, 3)),
    "The `data` parameter must be a `data.frame`."
  )
  expect_error(
    soil_color(data = "test"),
    "The `data` parameter must be a `data.frame`."
  )
})

#
soil_refle_test <- soil_refle[,-1]

test_that("Test if 'wavelength' column does not exist", {
  expect_error(
    soil_color(soil_refle_test),
    "The `data` does not contain the `wavelength` column."
  )
})

#
soil_refle_test <- soil_refle
soil_refle_test$wavelength_nm2 <- soil_refle$wavelength_nm

test_that("Test if there is more than one column with 'wavelength", {

  expect_error(
    soil_color(soil_refle_test),
    "The `data` contains more than one `wavelength` column."
  )
})

#
soil_refle_test <- soil_refle

test_that("Check if 'tri_values' argument exists", {

  expect_error(
    soil_color(soil_refle_test, tri_values = "none"),
    "The parameter `tri_values` are required."
  )
})

#
soil_refle_test <- soil_refle

test_that("Check if output has the correct structure", {
  expect_equal(nrow(soil_color(soil_refle_test)), 23)
  expect_equal(ncol(soil_color(soil_refle_test)), 9)
  expect_equal(
    names(soil_color(soil_refle_test)),
    c(
      "sample", "munsell", "H",
      "V", "C", "R",
      "G", "B", "hex"
    )
  )
})

#
soil_refle_test <- soil_refle
reference_values <- c("#B39272", "#AD8A67", "#A0785E", "#907E6D",
                      "#A88B70", "#B58363", "#9E8065", "#AB7B5F",
                      "#A48265", "#BAA189", "#B0977C", "#A28367",
                      "#A78C6E", "#C8A07C", "#9A7056", "#95806B",
                      "#907B66", "#A98E74", "#977158", "#A97C5E",
                      "#A8815E", "#9D8168", "#947D66")

test_that("Column values match the reference", {
  expect_equal(soil_color(soil_refle_test)$hex, reference_values)
})

#
soil_refle_test <- soil_refle
soil_refle_test$a1[4000:4200] <- NA

test_that("Test for missing values in the data", {
  result <- soil_color(soil_refle_test)
  expect_false("a1" %in% result$sample)
})

#
soil_refle_test <- soil_refle
color_test <- try(
  soil_color(data = soil_refle_test,
             name_wave = "WaVE",
             tri_values = "std",
             plot = TRUE),
  silent = TRUE)

test_that("Test all arguments", {

  expect_false(inherits(color_test, "try-error"))

})

#
soil_refle_test <- soil_refle
color_test <- try(
  soil_color(data = soil_refle_test,
             name_wave = "WaVE",
             tri_values = "xyz1931.1nm",
             plot = FALSE),
  silent = TRUE)

test_that("Test tri values", {

  expect_false(inherits(color_test, "try-error"))

})
#
color_test <- try(
  soil_color(data = soil_refle_test,
             name_wave = "WaVE",
             tri_values = "xyz1931.5nm",
             plot = FALSE),
  silent = TRUE)

test_that("Test tri values", {

  expect_false(inherits(color_test, "try-error"))

})
#
color_test <- try(
  soil_color(data = soil_refle_test,
             name_wave = "WaVE",
             tri_values = "xyz1964.1nm",
             plot = FALSE),
  silent = TRUE)

test_that("Test tri values", {

  expect_false(inherits(color_test, "try-error"))

})
#
color_test <- try(
  soil_color(data = soil_refle_test,
             name_wave = "WaVE",
             tri_values = "xyz1964.5nm",
             plot = FALSE),
  silent = TRUE)

test_that("Test tri values", {

  expect_false(inherits(color_test, "try-error"))

})

# output
#
test_that("Return is a data.frame", {
  expect_s3_class(
    soil_color(data = soil_refle_test),
    "data.frame"
  )
})

