
# Input
#
test_that("Check if 'data' argument exists", {
  expect_error(
    clean_sheet_cary(),
    "The parameter `data` are required."
  )
})

#
test_that("Check if 'prefix' argument exists", {
  expect_error(
    clean_sheet_cary(data_cary),
    "The parameter `prefix` are required."
  )
})

#
test_that("Test if input data is a data.frame", {
  expect_error(
    clean_sheet_cary(data = c(1, 2, 3)),
    "The `data` parameter must be a `data.frame`."
  )
  expect_error(
    clean_sheet_cary(data = "test"),
    "The `data` parameter must be a `data.frame`."
  )
})

#
data_cary_test <- data_cary[,seq(2,50, by = 2)]

test_that("Test if 'wavelength' column does not exist", {
  expect_error(
    clean_sheet_cary(data = data_cary_test, prefix = "x"),
    "Attention! The column with the wavelength was not found."
  )
})

#
data_cary_test <- data_cary
# data_cary_test$a1
data_cary_test$x2[20] <- NA
data_cary_test$x50[20] <- NA

test_that("Test for missing values in the data", {
  result <- clean_sheet_cary(data_cary_test, prefix = "x")
  expect_false("a1" %in% colnames(result))
  expect_false("a26" %in% colnames(result))
  })

#
data_cary_test <- data_cary[,c(1,3:50)]

test_that("Function runs without errors and returns expected output", {
  result <- try( clean_sheet_cary(data_cary_test, prefix = "x"), silent = TRUE)

  expect_false(inherits(result, "try-error"))

})

# dim(data_cary)
data_cary_test <- data_cary[1:4000,]

test_that("Check wavelength limits", {
  expect_error(
    clean_sheet_cary(data_cary_test, prefix = "x"),
    "The wavelength limits are different from the specified ones!"
  )
})

# output
#
test_that("Return is a data.frame", {
  expect_s3_class(
    clean_sheet_cary(data = data_cary, prefix = "x"),
    "data.frame"
  )
})

#
test_that("Test if 'wavelength' column is absent in the output",{
  expect_true("wavelength_nm" %in% colnames(clean_sheet_cary(data = data_cary, prefix = "x")))
})


