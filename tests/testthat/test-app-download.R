# download_helpers.R is a shiny-free helper sourced by the app; source it
# directly. Under R CMD check the package is installed, so it lands at
# system.file("app", ...); under devtools::test() the source tree is used and
# the relative path resolves. Try the installed location first, then source.
.p <- system.file("app", "download_helpers.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p)) {
  .p <- testthat::test_path("..", "..", "inst", "app", "download_helpers.R")
}
source(.p, local = TRUE)

test_that(".plot_device resolves every offered format", {
  expect_identical(.plot_device("png"), "png")
  expect_identical(.plot_device("jpg"), "jpeg")
  expect_identical(.plot_device("jpeg"), "jpeg")
  expect_true(is.function(.plot_device("pdf")))   # cairo_pdf
  expect_true(is.function(.plot_device("svg")))   # svglite or grDevices::svg
})

test_that(".plot_device rejects unknown formats", {
  expect_error(.plot_device("bmp"), "Unknown plot format")
})

test_that(".transparent_theme clears the opaque background rects", {
  th <- .transparent_theme()
  expect_s3_class(th, "theme")
  for (el in c("plot.background", "panel.background",
               "legend.background", "legend.box.background")) {
    rect <- th[[el]]
    expect_s3_class(rect, "element_rect")
    expect_identical(rect$fill, "transparent")
  }
})

test_that("a transparent export actually has alpha pixels", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("png")
  skip_if(!isTRUE(capabilities("png")), "no PNG device")

  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()                     # opaque white background by default

  f <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(f, p + .transparent_theme(), bg = "transparent",
                  width = 2, height = 2, dpi = 72)

  img <- png::readPNG(f)
  # An RGBA image (4th channel = alpha) with at least one fully transparent
  # pixel confirms the background came through see-through.
  expect_identical(dim(img)[3], 4L)
  expect_true(any(img[, , 4] == 0))
})
