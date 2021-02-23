context("Focusing")


test_that("focus_point calculates in easy situations", {
  expect_equal(
    focus_point(
      vector3(c(0,0), c(0, 2), c(0, 0)),
      vector3(c(1,0), c(0, 0), c(0, 1))
    ),
    vector3(0, 1, 0)
  )

  expect_equal(
    focus_point(
      vector3(c(0,0), c(0, 0), c(0, 0)),
      vector3(c(1,0), c(0, 0), c(0, 1))
    ),
    vector3(0, 0, 0)
  )

  expect_equal(
    focus_point(
      vector3(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)),
      vector3(c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
    ),
    vector3(0.5, 0.5, 0.5)
  )
})

test_that("focus_point calculates in more difficult situations", {
})
