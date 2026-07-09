# Configure chromote to launch Chrome/Chromium with flags that work in
# sandboxed, container, WSL, and CI environments. The default sandbox commonly
# hangs there (e.g. AppDriver$new() never reaching "idle"); --no-sandbox plus
# the /dev/shm workaround resolves it.
#
# Only runs when a browser is actually present and we are not on CRAN, so it is
# a no-op in environments that would skip the browser test anyway (and never
# spawns Chrome where there is none).

if (requireNamespace("chromote", quietly = TRUE) &&
    !is.null(chromote::find_chrome()) &&
    identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
  try(
    chromote::set_default_chromote_object(
      chromote::Chromote$new(
        browser = chromote::Chrome$new(
          args = c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
        )
      )
    ),
    silent = TRUE
  )
}
