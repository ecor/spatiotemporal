# Running the HoloViz explorer from RStudio Server (`run_app.R`)

`run_app.R` provides a few R functions to start, open and stop the
HoloViz/Panel explorer (`app.py`) directly from an **RStudio Server** session.

The app is reached through the **RStudio Server proxy** (port 8787), so:

- no extra port has to be opened in the server firewall (`ufw`) or in the
  hosting provider's firewall;
- no root / `sudo` access is needed;
- only users logged into RStudio Server can see the app.

## Requirements

- Python with the packages in `requirements.txt` (`panel`, `holoviews`,
  `hvplot`, `pandas`, `pyarrow`, …):

  ```bash
  cd inst/holoviz
  pip install -r requirements.txt
  ```

- R packages `rstudioapi` (to build the proxy URL) and, optionally,
  `reticulate` (to find the Python interpreter automatically).
- A database in Parquet format (see `export_db_parquet.R` or `stdb.py`), for
  example `inst/shiny/gsodapp/ext/db_icpac_v2_parquet`.

## Quick start

In the RStudio console on the server:

```r
source("~/local/rpackages/jrc/spatiotemporal/inst/holoviz/run_app.R")

start_spatiotemporal_app()   # starts Panel and opens the app in the browser
spatiotemporal_app_log()     # shows the last lines of the Panel log
stop_spatiotemporal_app()    # stops the app
```

`start_spatiotemporal_app()` prints and opens a URL such as

```
http://rendena103.cloud:8787/p/xxxxxxxx/app-spatiotemporal
```

The URL can be bookmarked: it stays valid as long as the app runs on the
same local port.

## Functions

### `start_spatiotemporal_app()`

Starts the Panel server in the background and opens the app.

```r
start_spatiotemporal_app(
  data    = ".../inst/shiny/gsodapp/ext/db_icpac_v2_parquet",
  port    = 5006,
  app     = "app-spatiotemporal",
  python  = NULL,
  open    = TRUE,
  restart = TRUE,
  timeout = 120
)
```

| Argument  | Description |
|-----------|-------------|
| `data`    | Database to load: a Parquet folder (recommended), or an `.rda` / `.rds` file (converted to Parquet on first use, slower). |
| `port`    | Local port used by Panel on the server (not exposed to the outside). |
| `app`     | App name, i.e. the last part of the URL. A symbolic link `<app>.py -> app.py` is created if missing. |
| `python`  | Python interpreter with Panel installed. If `NULL`: `panel` on the `PATH`, then the Python found by `reticulate`, then `python3`. |
| `open`    | If `TRUE`, opens the URL with `browseURL()`. |
| `restart` | If `TRUE` and the port is already in use, the running instance is stopped first; if `FALSE`, an error is raised. |
| `timeout` | Seconds to wait for the Panel server to answer. |

What it does:

1. creates the link `app-spatiotemporal.py -> app.py` if needed;
2. stops any process already listening on `port` (when `restart = TRUE`);
3. builds the RStudio proxy URL with
   `rstudioapi::translateLocalUrl("http://localhost:<port>/<app>", absolute = TRUE)`
   and allows its host (e.g. `rendena103.cloud:8787`) as websocket origin,
   together with `localhost:<port>`;
4. launches `panel serve` with `nohup`, so the app keeps running if the R
   session is restarted;
5. waits until the server answers, then prints and opens the URL.

Returns the URL (invisibly).

### `stop_spatiotemporal_app(port = 5006)`

Stops the Panel server listening on `port` (SIGTERM, then SIGKILL if needed).
Returns `TRUE` if the port is free afterwards.

### `spatiotemporal_app_log(port = 5006, n = 30)`

Prints the last `n` lines of the Panel log, stored in
`~/.cache/spatiotemporal_panel_<port>.log`. Useful when the app does not
start or the page stays blank.

### `spatiotemporal_app_url(port = 5006, app = "app-spatiotemporal")`

Returns the RStudio proxy URL of the app without starting anything (or the
`localhost` URL when called outside RStudio).

## Examples

```r
# another database
start_spatiotemporal_app(data = "~/data/my_db_parquet")

# two apps at the same time, on different ports
start_spatiotemporal_app(port = 5006)
start_spatiotemporal_app(data = "~/data/other_db_parquet", port = 5007,
                         app = "app-other")

# explicit Python interpreter (e.g. a virtualenv not visible to RStudio)
start_spatiotemporal_app(python = "~/.virtualenvs/r-reticulate/bin/python")

# start without opening the browser, get the URL
url <- start_spatiotemporal_app(open = FALSE)
url
```

## Troubleshooting

| Symptom | Cause / fix |
|---------|-------------|
| `Panel did not answer within 120 s` | Look at `spatiotemporal_app_log()`. Usually Python/Panel not found (use `python = ...`) or a wrong `data` path. Large `.rda`/`.rds` files take minutes on first load: convert them to Parquet first or increase `timeout`. |
| Page loads but plots stay empty | The websocket origin was refused: the log shows `Refusing websocket connection from Origin '...'`. Open the app with the URL returned by the function, not a different host name. |
| `port ... already in use` | Another app (or an old instance) uses the port: call `stop_spatiotemporal_app(port)` or use `restart = TRUE` (default). |
| Blank basemap | Tiles are downloaded by the browser: the provider may be blocked by the local network. Choose another basemap in the sidebar, or add one with the `STDB_TILES` environment variable (see `README.md`). |
| URL shows `localhost` instead of `:8787/p/...` | The function was not run inside RStudio (e.g. `Rscript`). Use an SSH tunnel (`ssh -N -L 5006:localhost:5006 user@server`) or run it from the RStudio console. |
