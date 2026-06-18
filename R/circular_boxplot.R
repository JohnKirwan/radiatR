# Circular boxplot (Buttarazzi, Pandolfo & Porzio 2018, Biometrics;
# doi:10.1111/biom.12889). Algorithm reimplemented from their bpDir package
# (GPL) in radiatR style: work in signed offset-from-median coordinates
# rel in (-pi, pi], so the antimedian sits at the +/-pi branch cut and the
# "fences must not pass the antimedian" rule is a clamp to +/-pi.

# Closed-form fence multiplier: at the sample's estimated von Mises
# concentration, the factor c such that Q3 + c*IQR lands on the 99.65% quantile.
.circ_box_constant <- function(tc) {
  conc <- as.numeric(circular::A1inv(circular::rho.circular(tc)))
  # As kappa -> Inf the von Mises quantile ratio converges to the Gaussian
  # Tukey value (~1.5). Cap concentration at a large finite value so tightly
  # clustered data (rho rounding to 1, A1inv = Inf) does not yield NaN. The cap
  # is 1e3 rather than 1e4 because circular::qvonmises loses accuracy past ~5e3
  # (its quantiles saturate, collapsing the constant to ~0); at 1e3 the constant
  # is a clean ~1.4997.
  if (!is.finite(conc)) conc <- 1e3
  conc <- min(conc, 1e3)
  qk <- function(p) {
    q <- as.numeric(circular::qvonmises(p, mu = circular::circular(0), kappa = conc))
    ((q + pi) %% (2 * pi)) - pi               # signed arc from 0
  }
  iqr <- qk(0.75) - qk(0.25)
  list(constant = (qk(0.9965) - qk(0.75)) / iqr, kappa = conc)
}

.circ_boxplot_core <- function(theta, axial) {
  n <- length(theta)
  na_spec <- function(reason, drawable = FALSE)
    list(median = NA_real_, antimedian = NA_real_, hinges = c(NA_real_, NA_real_),
         box_arc = c(NA_real_, NA_real_), constant = NA_real_, kappa = NA_real_,
         fences = c(NA_real_, NA_real_), whiskers = c(NA_real_, NA_real_),
         far_out = numeric(0), n = n, axial = axial,
         drawable = drawable, reason = reason)

  if (n < 4L) return(na_spec("fewer than 4 observations; boxplot not drawn"))

  tc <- circular::circular(theta %% (2 * pi), units = "radians",
                           modulo = "2pi", zero = 0, rotation = "counter")
  M  <- suppressWarnings(as.numeric(circular::median.circular(tc)))
  if (!is.finite(M)) return(na_spec("circular median is not unique; boxplot not drawn"))
  M  <- M %% (2 * pi)

  # signed offsets from the median; drop ties exactly at the antimedian (|rel|=pi)
  rel  <- ((theta - M + pi) %% (2 * pi)) - pi
  rel  <- sort(rel[abs(abs(rel) - pi) > 1e-9])
  m    <- length(rel)
  if (m < 4L) return(na_spec("fewer than 4 usable observations; boxplot not drawn"))

  # Tukey outer-inward depth: rank from both ends (the antimedian) toward 0.
  dm <- round((1 + m) / 2 - 0.1)        # depth of the median (bpDir)
  dq <- (1 + dm) / 2                    # depth of the quartiles (may be x.5)
  lo <- function(k) rel[k]              # clockwise hinge side (rel < 0)
  hi <- function(k) rel[m + 1 - k]      # anticlockwise hinge side (rel > 0)
  if (dq %% 1 == 0) {
    h_lo <- lo(dq); h_hi <- hi(dq)
  } else {
    h_lo <- mean(c(lo(dq - 0.5), lo(dq + 0.5)))
    h_hi <- mean(c(hi(dq - 0.5), hi(dq + 0.5)))
  }
  d <- h_hi - h_lo                      # IQR arc length

  cc      <- .circ_box_constant(tc)
  f_hi    <- min(h_hi + cc$constant * d,  pi)   # clamp at the antimedian
  f_lo    <- max(h_lo - cc$constant * d, -pi)

  inside_hi <- rel[rel >= h_hi & rel <= f_hi]
  inside_lo <- rel[rel <= h_lo & rel >= f_lo]
  w_hi <- if (length(inside_hi)) max(inside_hi) else h_hi
  w_lo <- if (length(inside_lo)) min(inside_lo) else h_lo
  far  <- rel[rel > w_hi | rel < w_lo]

  abs_of <- function(x) (M + x) %% (2 * pi)

  # advisory: near-uniform / antipodally symmetric (box ~ half-circle, c ~ 0.5)
  reason <- NA_character_
  if (is.finite(cc$constant) && (d >= 0.95 * pi || cc$constant <= 0.55))
    reason <- "near-uniform or antipodal: box spans ~half the circle; interpretation not recommended"

  list(median     = M,
       antimedian = (M + pi) %% (2 * pi),
       hinges     = abs_of(c(h_lo, h_hi)),
       box_arc    = abs_of(c(h_lo, h_hi)),
       constant   = cc$constant,
       kappa      = cc$kappa,
       fences     = abs_of(c(f_lo, f_hi)),
       whiskers   = abs_of(c(w_lo, w_hi)),
       far_out    = if (length(far)) abs_of(far) else numeric(0),
       n = m, axial = axial, drawable = TRUE, reason = reason)
}

# Axial boxplot: double the axes (mod pi -> 2pi), run the core where the
# distribution is unimodal (so the constant/kappa are taken there, per the
# paper's 4.4), then halve every location back to [0, pi).
.circ_boxplot_axial <- function(theta) {
  s <- .circ_boxplot_core((2 * theta) %% (2 * pi), axial = TRUE)
  if (!isTRUE(s$drawable)) return(s)
  halve <- function(x) (x / 2) %% pi
  s$median     <- halve(s$median)
  s$antimedian <- halve(s$antimedian)
  s$hinges     <- halve(s$hinges)
  s$box_arc    <- halve(s$box_arc)
  s$fences     <- halve(s$fences)
  s$whiskers   <- halve(s$whiskers)
  s$far_out    <- if (length(s$far_out)) halve(s$far_out) else numeric(0)
  s
}

#' Circular boxplot statistics (Tukey-like, for circular and axial data)
#'
#' Computes the five-number summary and fences of a circular boxplot following
#' Buttarazzi, Pandolfo & Porzio (2018): observations are depth-ranked outward
#' from the antimedian, the box spans the central ~50% (hinge to hinge through
#' the median), and the fence multiplier is obtained in closed form from the
#' von Mises quantiles at the sample's estimated concentration (so ~0.7% of
#' observations are flagged as far-out under that reference). Pairs with
#' [add_circular_boxplot()] for rendering.
#'
#' @param hd A data frame with a column of heading angles in radians
#'   (unit-circle convention), or a numeric vector of angles.
#' @param angle_col Name of the angle column when `hd` is a data frame.
#'   Default `"heading"`.
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi):
#'   angles are doubled, the boxplot computed, and locations halved back to
#'   `[0, pi)`; the fence multiplier is taken on the doubled data. Default
#'   `FALSE`.
#' @return A list with `median`, `antimedian`, `hinges`, `box_arc`,
#'   `constant`, `kappa`, `fences`, `whiskers`, `far_out` (all radians,
#'   unit-circle convention), `n`, `axial`, `drawable`, and `reason`. When
#'   `drawable` is `FALSE` (non-unique median or `n < 4`) the location fields
#'   are `NA`; `reason` may also carry a non-fatal advisory while `drawable`
#'   remains `TRUE` (near-uniform data).
#' @references Buttarazzi, D., Pandolfo, G. & Porzio, G. C. (2018). A boxplot
#'   for circular data. \emph{Biometrics} 74(4), 1492--1501.
#'   \doi{10.1111/biom.12889}
#' @source Algorithm reimplemented from the \pkg{bpDir} package.
#' @seealso [add_circular_boxplot()], [circ_summarise()], [vonmises_fit()]
#' @importFrom circular circular median.circular A1inv rho.circular qvonmises
#' @export
circ_boxplot_stats <- function(hd, angle_col = "heading", axial = FALSE) {
  theta <- if (is.data.frame(hd)) {
    if (!angle_col %in% names(hd))
      stop("`angle_col` '", angle_col, "' not found in `hd`.")
    hd[[angle_col]]
  } else hd
  theta <- as.numeric(theta)
  theta <- theta[is.finite(theta)]
  if (isTRUE(axial)) return(.circ_boxplot_axial(theta))   # implemented in Task 2
  .circ_boxplot_core(theta, axial = FALSE)
}
