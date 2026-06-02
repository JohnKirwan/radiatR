#' @importFrom grDevices as.raster
#' @importFrom stats aggregate setNames
#' @importFrom utils head tail
#' @keywords internal
utils::globalVariables(c(
  "x", "y", "xend", "yend", "..grp", "..row",
  ".rose_grp", ".kde_grp", ".vm_grp", ".cr_grp", ".v_grp"
))
