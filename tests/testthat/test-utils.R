test_that("dot() calculates the dot product of two regular vectors",
          {
            expect_equal(
              dot(c(1, 10, 100), c(-1, 0, 1)),
              99)
          })