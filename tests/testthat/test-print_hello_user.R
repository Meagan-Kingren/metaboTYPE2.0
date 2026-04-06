test_that("print_hello_user prints hello user", {
  expect_equal(
    print_hello_user(x = "user"),
    "Hello, user!"
  )
})
