test_that("import_data works", {
  #paths to test data
  test_paths <- list(
    customers = "test_data/customers.xlsx",
    policies = "test_data/policies.xlsx",
    claims ="test_data/claims.xlsx"
  )

  #add incorrect fileptath
  expect_error(import_data("fake.xlsx", test_paths$policies, test_paths$claims))

  #test
  result <- import_data(test_paths$customers, test_paths$policies, test_paths$claims)
  expect_type(result, "list")
  expect_named(result,c("customers", "policies", "claims"))
  expect_s3_class(result$customers, "data.frame")
  expect_s3_class(result$policies, "data.frame")
  expect_s3_class(result$claims, "data.frame")
})
