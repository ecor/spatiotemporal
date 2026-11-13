## Launch / stop the HoloViz (Panel) explorer from an RStudio Server session
## and open it through the RStudio proxy (port 8787), without opening ports.
##
## Usage (RStudio console on the server):
##   source("~/local/rpackages/jrc/spatiotemporal/inst/holoviz/run_app.R")
##   url <- start_spatiotemporal_app()      # starts Panel, prints + opens the URL
##   stop_spatiotemporal_app()              # stops it
##   spatiotemporal_app_log()               # last lines of the Panel log
##
## Options (arguments or env vars):
##   data   : Parquet folder / .rda / .rds        (default: db_icpac_v2_parquet)
##   port   : local port of Panel                 (default 5006)
##   python : python with panel installed         (default: `panel` on PATH,
##            then the reticulate python, then "python3")

.st_app_dir <- local({
  d <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(d) || !file.exists(file.path(d, "app.py")))
    d <- path.expand("~/local/rpackages/jrc/spatiotemporal/inst/holoviz")
  d
})

.st_log <- function(port) {
  d <- file.path(path.expand("~"), ".cache")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.path(d, sprintf("spatiotemporal_panel_%d.log", port))
}

.st_port_busy <- function(port) {
  con <- suppressWarnings(tryCatch(
    socketConnection("127.0.0.1", port = port, open = "r+", blocking = TRUE, timeout = 1),
    error = function(e) NULL))
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

.st_kill_port <- function(port, signal = "TERM") {
  if (nzchar(Sys.which("fuser"))) {
    system2("fuser", c("-k", paste0("-", signal), sprintf("%d/tcp", port)), stdout = FALSE, stderr = FALSE)
  } else {
    system2("pkill", c(paste0("-", signal), "-f", shQuote(sprintf("panel serve .*--port %d", port))),
            stdout = FALSE, stderr = FALSE)
  }
}

.st_panel_cmd <- function(python = NULL) {
  if (!is.null(python)) return(c(python, "-m", "panel"))
  p <- Sys.which("panel")
  if (nzchar(p)) return(unname(p))
  if (requireNamespace("reticulate", quietly = TRUE)) {
    py <- tryCatch(reticulate::py_discover_config()$python, error = function(e) "")
    if (nzchar(py)) return(c(py, "-m", "panel"))
  }
  c("python3", "-m", "panel")
}

## RStudio proxy URL of a local port, e.g. http://rendena103.cloud:8787/p/abcd1234/
spatiotemporal_app_url <- function(port = 5006, app = "app-spatiotemporal") {
  local_url <- sprintf("http://localhost:%d/%s", port, app)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::translateLocalUrl(local_url, absolute = TRUE)
  } else {
    local_url
  }
}

start_spatiotemporal_app <- function(
    data   = file.path(.st_app_dir, "..", "shiny", "gsodapp", "ext", "db_icpac_v2_parquet"),
    port   = 5006,
    app    = "app-spatiotemporal",
    python = NULL,
    open   = TRUE,
    restart = TRUE,
    timeout = 120) {

  app_file <- file.path(.st_app_dir, paste0(app, ".py"))
  if (!file.exists(app_file)) {                    # app-spatiotemporal.py -> app.py
    file.symlink("app.py", app_file)
  }
  data <- normalizePath(data, mustWork = TRUE)

  if (.st_port_busy(port)) {
    if (!restart) stop("port ", port, " already in use (restart = FALSE)")
    stop_spatiotemporal_app(port)
  }

  url <- spatiotemporal_app_url(port, app)
  origin <- sub("^https?://([^/]+).*$", "\\1", url)          # e.g. rendena103.cloud:8787
  origins <- unique(c(origin, sprintf("localhost:%d", port)))

  cmd <- .st_panel_cmd(python)
  args <- c(cmd[-1], "serve", shQuote(app_file), "--port", port,
            paste0("--allow-websocket-origin=", origins),
            "--args", "--data", shQuote(data))
  log <- .st_log(port)
  message("Starting: ", paste(c(cmd[1], args), collapse = " "))
  ## nohup: Panel keeps running if the R session is restarted
  system2("nohup", c(shQuote(cmd[1]), args), stdout = log, stderr = log, wait = FALSE)

  ## wait until the Panel server answers
  t0 <- Sys.time()
  repeat {
    status <- suppressWarnings(tryCatch(
      attr(curlGetHeaders(sprintf("http://127.0.0.1:%d/%s", port, app)), "status"),
      error = function(e) NA))
    if (isTRUE(status %in% c(200, 405))) break   # 405 = server up (HEAD not allowed)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      message(paste(utils::tail(readLines(log, warn = FALSE), 20), collapse = "\n"))
      stop("Panel did not answer within ", timeout, " s (log: ", log, ")")
    }
    Sys.sleep(2)
  }
  message("App running: ", url, "\nLog: ", log)
  if (open) utils::browseURL(url)
  invisible(url)
}

stop_spatiotemporal_app <- function(port = 5006) {
  .st_kill_port(port, "TERM")
  for (i in 1:10) {
    if (!.st_port_busy(port)) return(invisible(TRUE))
    Sys.sleep(0.5)
  }
  .st_kill_port(port, "KILL")
  Sys.sleep(1)
  invisible(!.st_port_busy(port))
}

spatiotemporal_app_log <- function(port = 5006, n = 30) {
  cat(utils::tail(readLines(.st_log(port), warn = FALSE), n), sep = "\n")
}
