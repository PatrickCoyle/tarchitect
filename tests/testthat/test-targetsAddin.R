test_that("a test test", {
  # Create a temporary targets file
  tmp1 <- targets::tar_dir({
    targets::tar_script(
      code = {
        list(
          tar_target(y1, 1 + 1),
          tar_target(y2, 1 + 1),
          tar_target(z, y1 + y2)
        )
      }
    )
    targets::tar_make(z)
    return(targets::tar_read(z))
  })
  expect_equal(tmp1, 4)
})
