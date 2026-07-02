testthat::test_that("count_names and basic helpers work correctly", {
  V <- c("a", "b", "c")
  X <- c("b", "a", "b", "d")

  out <- Rsimcity::count_names(V, X)
  testthat::expect_named(out, V)
  testthat::expect_equal(unname(out), c(1L, 2L, 0L))
})


testthat::test_that("bind_list_cols repeats scalar values and ignores non-scalars", {
  df <- data.frame(y = 1:3, stringsAsFactors = FALSE)
  x <- list(a = 10, b = "x", c = 1:2, d = list(1,2))

  res <- Rsimcity:::bind_list_cols(df, x)

  testthat::expect_true(is.data.frame(res))
  testthat::expect_true(all(c("a", "b", "y") %in% names(res)))
  testthat::expect_equal(res$a, rep(10, 3))
  testthat::expect_equal(res$b, rep("x", 3))
})


testthat::test_that("numColMean computes numeric means and keeps first for characters", {
  df <- data.frame(
    num1 = c(1, 2, NA),
    num2 = c(3, 3, 3),
    chr = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  out <- Rsimcity:::numColMean(df)

  testthat::expect_true(is.data.frame(out))
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$num1, mean(c(1,2), na.rm = TRUE))
  testthat::expect_equal(out$num2, 3)
  testthat::expect_equal(out$chr, "a")
})
