"""
HoloViz (Panel + HoloViews + hvPlot) explorer for the `spatiotemporal` database.

Run:
    panel serve app.py --show --args --data ../../data/db.rda
    panel serve app.py --show --args --data ../../data/db_parquet      # Parquet cache
    STDB_DATA=/path/to/db_icpac_v2.rds panel serve app.py --show

An .rda/.rds is converted once to a "<name>_parquet" folder next to it;
later starts read the Parquet cache in ~1 s.
"""

from __future__ import annotations

import argparse
import io
import os
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import holoviews as hv
import hvplot.pandas  # noqa: F401  (registers .hvplot)
import panel as pn
from holoviews.util.transform import lon_lat_to_easting_northing

import stdb

hv.extension("bokeh")
pn.extension("tabulator", sizing_mode="stretch_width", notifications=True)

HERE = Path(__file__).resolve().parent
DEFAULT_DATA = HERE.parent.parent / "data" / "db.rda"

# --------------------------------------------------------------------------- #
# Data
# --------------------------------------------------------------------------- #
def _data_path() -> Path:
    ap = argparse.ArgumentParser(add_help=False)
    ap.add_argument("--data", default=os.environ.get("STDB_DATA"))
    a, _ = ap.parse_known_args(sys.argv[1:])
    if a.data:
        return Path(a.data)
    pq = DEFAULT_DATA.with_name("db_parquet")
    return pq if pq.exists() else DEFAULT_DATA


@pn.cache
def get_db(path: str) -> stdb.SpatioTemporalDB:
    return stdb.load(path)


DB = get_db(str(_data_path()))
ST_LABEL = DB.station_label()                    # p_id -> name
VAR_LABEL = DB.variable_label()                  # variable_id -> name
VAR_ID = {v: k for k, v in VAR_LABEL.items()}
PROVIDERS = dict(zip(DB.provider["name"].astype(str), DB.provider["ID"].astype(int)))
VAR_DESC = dict(zip(DB.variable["name"].astype(str), DB.variable["description"].astype(str)))
TMIN, TMAX = DB.ts["timestamptz"].min(), DB.ts["timestamptz"].max()

P = DB.p.copy()
P["ID"] = P["ID"].astype(int)
P["station"] = P["ID"].map(ST_LABEL)
P["easting"], P["northing"] = lon_lat_to_easting_northing(P["lon"].to_numpy(), P["lat"].to_numpy())
P["provider"] = P["provider_id"].map({v: k for k, v in PROVIDERS.items()}).astype(str)
_desc = P["description"].map(stdb._parse_description) if "description" in P else None
for k in ("COUNTRY", "ELEVATION"):
    if _desc is not None:
        P[k.lower()] = _desc.map(lambda d: d.get(k, ""))

# variables whose period aggregate is a total rather than an average
SUM_VARS = {v for v in VAR_ID
            if v.upper() == "PRCP" or v.upper().startswith("I_") or re.search(r"prec|rain", v, re.I)}

# --------------------------------------------------------------------------- #
# Widgets
# --------------------------------------------------------------------------- #
w_provider = pn.widgets.MultiChoice(name="Provider", options=list(PROVIDERS), value=list(PROVIDERS))
default_var = "PRCP" if "PRCP" in VAR_ID else next(iter(VAR_ID))
w_variable = pn.widgets.Select(name="Variable", options=sorted(VAR_ID), value=default_var)
w_compare = pn.widgets.MultiChoice(name="Compare with (same station)", options=sorted(VAR_ID), value=[])
w_stations = pn.widgets.MultiChoice(
    name="Stations (or click / box-select on the map)",
    options={ST_LABEL[i]: i for i in sorted(ST_LABEL, key=lambda i: ST_LABEL[i])},
    value=[], max_items=12, search_option_limit=200,
)
w_dates = pn.widgets.DatetimeRangePicker(
    name="Period", value=(TMIN.to_pydatetime(), TMAX.to_pydatetime()),
    start=TMIN.to_pydatetime(), end=TMAX.to_pydatetime(), enable_time=False,
)
w_agg = pn.widgets.RadioButtonGroup(name="Time step", options=["Daily", "Monthly", "Yearly"],
                                    value="Monthly", button_type="primary", button_style="outline")
w_cover = pn.widgets.FloatSlider(name="Min. valid days per period (fraction)", start=0, end=1,
                                 step=0.05, value=0.8)
w_attr = pn.widgets.MultiChoice(name="Keep only attribute values (empty = all)", options=[], value=[])
w_minyears = pn.widgets.IntSlider(name="Map: min. valid days to count a year", start=1, end=366, value=300)


def _update_attr_options(event=None):
    vid = VAR_ID[w_variable.value]
    vals = DB.ts.loc[DB.ts["variable_id"] == vid, "attrvalue"].dropna().unique().tolist()
    vals = sorted({str(v) for v in vals}, key=lambda s: (len(s), s))
    w_attr.options = vals[:200]
    w_attr.value = []


w_variable.param.watch(_update_attr_options, "value")
_update_attr_options()

w_variable_desc = pn.pane.Markdown(styles={"font-size": "0.85em", "color": "#666"})
w_variable_desc.object = pn.bind(lambda v: f"*{VAR_DESC.get(v, '')}*", w_variable)

# --------------------------------------------------------------------------- #
# Data selection helpers
# --------------------------------------------------------------------------- #
def _period():
    s, e = w_dates.value
    return pd.Timestamp(s), pd.Timestamp(e)


def select_ts(variables, stations, period, attrs):
    if not stations:
        return pd.DataFrame(columns=["timestamptz", "value", "attrvalue", "station", "variable"])
    s, e = period
    df = DB.get_ts(p_id=stations, variable_id=[VAR_ID[v] for v in variables], start=s, end=e,
                   attrvalue=attrs or None)
    df = df[["timestamptz", "p_id", "variable_id", "value", "attrvalue"]].copy()
    df["station"] = df["p_id"].astype(int).map(ST_LABEL)
    df["variable"] = df["variable_id"].astype(int).map(VAR_LABEL)
    return df


def aggregate(df, step, cover):
    if step == "Daily" or df.empty:
        return df.assign(n_valid=1)
    freq = {"Monthly": "MS", "Yearly": "YS"}[step]
    out = []
    for (st, var), g in df.groupby(["station", "variable"], observed=True):
        s = g.set_index("timestamptz")["value"]
        how = "sum" if var in SUM_VARS else "mean"
        r = s.resample(freq).agg([how, "count"]).rename(columns={how: "value", "count": "n_valid"})
        ndays = r.index.to_series().apply(
            lambda t: t.days_in_month if step == "Monthly" else (366 if t.is_leap_year else 365))
        r.loc[r["n_valid"] < cover * ndays.values, "value"] = np.nan
        r = r.reset_index().assign(station=st, variable=var)
        out.append(r)
    return pd.concat(out, ignore_index=True) if out else df


# --------------------------------------------------------------------------- #
# Map
# --------------------------------------------------------------------------- #
tiles = hv.element.tiles.CartoLight().opts(alpha=0.9)
_current_points = {"df": P}


def map_points(providers, variable, period, minyears):
    vid = VAR_ID[variable]
    s, e = period
    av = DB.availability
    av = av[(av.variable_id == vid) & (av.year >= s.year) & (av.year <= e.year)]
    nyears = av[av.n >= minyears].groupby("p_id").size()
    nobs = av.groupby("p_id")["n"].sum()
    df = P[P["provider"].isin(providers)].copy()
    df["years_with_data"] = df["ID"].map(nyears).fillna(0).astype(int)
    df["n_obs"] = df["ID"].map(nobs).fillna(0).astype(int)
    df = df.sort_values("years_with_data").reset_index(drop=True)
    _current_points["df"] = df
    vdims = ["years_with_data", "station", "ID", "n_obs", "lon", "lat", "provider"]
    vdims += [c for c in ("country", "elevation") if c in df]
    return hv.Points(df, kdims=["easting", "northing"], vdims=vdims).opts(
        color="years_with_data", cmap="viridis", colorbar=True, clabel=f"years with {variable} data",
        size=9, line_color="white", line_width=0.6,
        tools=["hover", "tap", "box_select"], nonselection_alpha=0.55,
        hover_tooltips=[("station", "@station"), ("p_id", "@ID"), ("years", "@years_with_data"),
                        ("obs", "@n_obs"), ("lon/lat", "@lon{0.000}, @lat{0.000}")],
    )


def map_highlight(stations):
    d = P[P["ID"].isin(stations)]
    return hv.Points(d, kdims=["easting", "northing"], vdims=["station"]).opts(
        marker="circle", size=17, fill_alpha=0, line_color="#d62728", line_width=2.5)


def map_labels(stations):
    d = P[P["ID"].isin(stations)].assign(lab=lambda x: x["station"].str.extract(r"\((.*)\)")[0].fillna(x["station"]))
    return hv.Labels(d, kdims=["easting", "northing"], vdims=["lab"]).opts(
        text_font_size="9pt", yoffset=40000, text_color="#d62728")


dm_points = hv.DynamicMap(pn.bind(map_points, w_provider, w_variable, pn.bind(lambda v: _period(), w_dates),
                                  w_minyears))
dm_sel = hv.DynamicMap(pn.bind(map_highlight, w_stations))
dm_lab = hv.DynamicMap(pn.bind(map_labels, w_stations))
sel_stream = hv.streams.Selection1D(source=dm_points)


def _on_select(index):
    if not index:
        return
    df = _current_points["df"]
    ids = [int(df.iloc[i]["ID"]) for i in index if i < len(df)]
    if len(ids) == 1:  # click: toggle one station
        cur = list(w_stations.value)
        i = ids[0]
        cur = [x for x in cur if x != i] if i in cur else cur + [i]
        w_stations.value = cur[-12:]
    else:              # box select: replace
        w_stations.value = ids[:12]


sel_stream.add_subscriber(_on_select)

station_map = (tiles * dm_points * dm_sel * dm_lab).opts(
    hv.opts.Overlay(height=520, responsive=True, active_tools=["wheel_zoom"], xaxis=None, yaxis=None,
                    title="Stations — colour = years with data"))

# --------------------------------------------------------------------------- #
# Plots
# --------------------------------------------------------------------------- #
def _empty(msg="Select one or more stations (dropdown or map)."):
    return pn.pane.Alert(msg, alert_type="info")


@pn.depends(w_variable, w_stations, w_dates, w_agg, w_cover, w_attr)
def ts_plot(variable, stations, dates, step, cover, attrs):
    if not stations:
        return _empty()
    df = aggregate(select_ts([variable], stations, _period(), attrs), step, cover)
    if df.empty:
        return _empty("No data for this selection.")
    kind = "step" if step != "Daily" and variable in SUM_VARS else "line"
    unit = {"Daily": "daily", "Monthly": "monthly", "Yearly": "yearly"}[step]
    how = "total" if variable in SUM_VARS else "mean"
    plot = df.hvplot(x="timestamptz", y="value", by="station", kind=kind, height=420, responsive=True,
                     xlabel="", ylabel=f"{variable} ({unit} {how})" if step != "Daily" else variable,
                     downsample=(step == "Daily"), legend="top_left", grid=True,
                     hover_cols=["n_valid"]).opts(title=f"{variable} — {step.lower()} series")
    return pn.pane.HoloViews(plot, linked_axes=False)


@pn.depends(w_variable, w_compare, w_stations, w_dates, w_agg, w_cover)
def compare_plot(variable, compare, stations, dates, step, cover):
    if not stations:
        return _empty()
    vars_ = [variable] + [v for v in compare if v != variable]
    if len(vars_) == 1:
        return _empty("Pick one or more variables in 'Compare with' (sidebar).")
    df = aggregate(select_ts(vars_, stations[:4], _period(), None), step, cover)
    if df.empty:
        return _empty("No data for this selection.")
    plots = [df[df.variable == v].hvplot(x="timestamptz", y="value", by="station", height=230,
                                         responsive=True, ylabel=v, xlabel="", legend="top_left",
                                         downsample=(step == "Daily"), grid=True).opts(title=v)
             for v in vars_]
    return pn.pane.HoloViews(hv.Layout(plots).cols(1).opts(shared_axes=True), linked_axes=True)


@pn.depends(w_variable, w_stations, w_dates, w_provider)
def availability_plot(variable, stations, dates, providers):
    vid = VAR_ID[variable]
    s, e = _period()
    av = DB.availability
    av = av[(av.variable_id == vid) & av.year.between(s.year, e.year)].copy()
    ids = stations or P.loc[P.provider.isin(providers), "ID"].tolist()
    av = av[av.p_id.isin(ids)]
    if av.empty:
        return _empty("No data for this selection.")
    av["station"] = av["p_id"].astype(int).map(ST_LABEL)
    order = av.groupby("station")["n"].sum().sort_values().index.tolist()
    h = max(300, min(18 * len(order) + 120, 2400))
    hm = hv.HeatMap(av, kdims=["year", "station"], vdims=["n"]).redim.values(station=order).opts(
        cmap="YlGnBu", colorbar=True, clabel="valid days", tools=["hover"], height=h, responsive=True,
        xrotation=45, title=f"{variable} — valid days per year ({'selected' if stations else 'all'} stations)",
        clim=(0, 366), line_color=None, yaxis="left", fontsize={"yticks": 7})
    return pn.pane.HoloViews(hm)


@pn.depends(w_variable, w_stations, w_dates)
def attribute_plot(variable, stations, dates):
    if not stations:
        return _empty()
    df = select_ts([variable], stations, _period(), None)
    if df.empty or df["attrvalue"].isna().all():
        return _empty("No attribute values for this variable.")
    d = df[df["attrvalue"].notna()]
    d = d.assign(attrvalue=d["attrvalue"].astype(object).map(str), year=d.timestamptz.dt.year.astype(str))
    levels = sorted(d["attrvalue"].unique(), key=lambda v: (len(v), v))
    pal = hv.Cycle.default_cycles["Category20"] if len(levels) > 10 else hv.Cycle.default_cycles["Category10"]
    cmap = {v: pal[i % len(pal)] for i, v in enumerate(levels)}
    tot = d.groupby(["station", "attrvalue"]).size().rename("n").reset_index()
    by_year = (d.groupby(["year", "attrvalue"]).size().unstack(fill_value=0)
                 .reindex(columns=levels, fill_value=0).stack().rename("n").reset_index())
    bars = tot.hvplot.barh(x="station", y="n", by="attrvalue", stacked=True, cmap=cmap, legend="right",
                           height=max(240, 110 + 45 * tot.station.nunique()), responsive=True, xlabel="",
                           ylabel="days", title="Attribute values per station")
    area = hv.Bars(by_year, kdims=["year", "attrvalue"], vdims="n").opts(
        stacked=True, cmap=cmap, color="attrvalue", height=320, responsive=True, legend_position="right",
        xrotation=90, ylabel="days", tools=["hover"], fontsize={"xticks": 7},
        title="Attribute values per year (selected stations)")
    legend = DB.attr_legend(variable)
    attr_desc = DB["attribute"] if "attribute" in DB.tables else None
    md = ""
    if attr_desc is not None:
        row = attr_desc[attr_desc["variable_name"] == variable]
        if len(row) and not legend:
            md = f"**{row['name'].iloc[0]}** — {row['description'].iloc[0]}"
    if legend:
        md = "**PRCP attributes (GSOD)**\n\n" + "\n".join(f"- **{k}**: {v}" for k, v in legend.items())
    return pn.Column(pn.pane.HoloViews(bars, linked_axes=False), pn.pane.HoloViews(area, linked_axes=False),
                     pn.pane.Markdown(md))


@pn.depends(w_variable, w_stations, w_dates, w_cover)
def climatology_plot(variable, stations, dates, cover):
    if not stations:
        return _empty()
    df = aggregate(select_ts([variable], stations, _period(), None), "Monthly", cover)
    if df.empty:
        return _empty("No data for this selection.")
    df = df.dropna(subset=["value"]).assign(month=lambda x: x.timestamptz.dt.month)
    clim = df.groupby(["station", "month"])["value"].agg(["mean", "std", "count"]).reset_index()
    line = clim.hvplot.line(x="month", y="mean", by="station", height=360, responsive=True, grid=True,
                            xticks=list(range(1, 13)), ylabel=f"{variable} monthly "
                            f"{'total' if variable in SUM_VARS else 'mean'}", legend="top_left",
                            hover_cols=["std", "count"], title=f"{variable} — mean annual cycle", line_width=2.5)
    df = df.sort_values("month").assign(month=lambda x: x["month"].map(lambda m: f"{m:02d}"))
    box = df.hvplot.box(y="value", by=["month"], height=300, responsive=True,
                        title="Spread of monthly values (all selected stations)", ylabel=variable)
    return pn.Column(pn.pane.HoloViews(line, linked_axes=False), pn.pane.HoloViews(box, linked_axes=False))


# --------------------------------------------------------------------------- #
# Table + download
# --------------------------------------------------------------------------- #
def _table_df(variable, compare, stations, dates, step, cover, attrs):
    vars_ = [variable] + [v for v in compare if v != variable]
    df = select_ts(vars_, stations, _period(), attrs)
    df = aggregate(df, step, cover)
    cols = [c for c in ["station", "variable", "timestamptz", "value", "attrvalue", "n_valid"] if c in df]
    return df[cols].sort_values(["station", "variable", "timestamptz"]).reset_index(drop=True)


def _table_df_now():
    return _table_df(w_variable.value, w_compare.value, w_stations.value, w_dates.value,
                     w_agg.value, w_cover.value, w_attr.value)


def table_view(*args):
    df = _table_df(*args)
    if df.empty:
        return _empty()
    return pn.widgets.Tabulator(df, pagination="remote", page_size=25, disabled=True,
                                header_filters=True, height=650, show_index=False)


def _csv():
    df = _table_df_now()
    buf = io.StringIO()
    df.to_csv(buf, index=False)
    buf.seek(0)
    return buf


download = pn.widgets.FileDownload(callback=_csv, filename="spatiotemporal_selection.csv",
                                   button_type="success", label="Download selection (CSV)")

# --------------------------------------------------------------------------- #
# Schema tab (like the nomnoml diagram in the R README)
# --------------------------------------------------------------------------- #
def schema_view():
    sch = DB.schema()
    cards = []
    for t in sch:
        rows = "".join(
            f"<tr><td><code>{c['column']}</code></td><td>{c['type']}</td>"
            f"<td>{('→ <b>' + c['references'] + '</b>.ID') if c['references'] else ''}</td></tr>"
            for c in t["columns"])
        cards.append(pn.Card(pn.pane.HTML(
            f"<table style='width:100%;font-size:0.85em'><tr><th align=left>column</th>"
            f"<th align=left>type</th><th align=left>FK</th></tr>{rows}</table>"),
            title=f"{t['table']}  ({t['rows']:,} rows)", collapsible=False, width=330, sizing_mode="fixed"))
    edges = [(t["table"], c["references"]) for t in sch for c in t["columns"] if c["references"]]
    nodes = sorted({x for e in edges for x in e})
    graph = hv.Graph((pd.DataFrame(edges, columns=["source", "target"]),
                      hv.Nodes(_circle_layout(nodes), ["x", "y", "index"]))).opts(
        directed=True, arrowhead_length=0.03, node_size=62, node_color="#4c78a8", edge_color="#999",
        edge_line_width=1.5, xaxis=None, yaxis=None, height=340, padding=0.15, responsive=True, title="Foreign keys (child → parent table)",
        tools=["hover"])
    labels = hv.Labels(_circle_layout(nodes), ["x", "y"], "index").opts(text_color="white", text_font_size="9pt")
    return pn.Column(pn.pane.Markdown(f"**Source:** `{DB.source}`"), graph * labels,
                     pn.FlexBox(*cards))


_LAYOUT = {"provider": (0, 2), "operator": (2, 2), "project": (3.2, 2),
           "p": (-1, 1), "variable": (0.6, 1), "attribute": (1.6, 1), "action": (2.6, 1), "ts": (0.8, 0)}


def _circle_layout(nodes):
    """Layered layout: lookup tables on top, ts (fact table) at the bottom."""
    extra = [n for n in nodes if n not in _LAYOUT]
    pos = dict(_LAYOUT)
    for i, n in enumerate(extra):
        pos[n] = (4 + i, 1)
    return pd.DataFrame({"x": [pos[n][0] for n in nodes], "y": [pos[n][1] for n in nodes], "index": nodes})


# --------------------------------------------------------------------------- #
# Station info card
# --------------------------------------------------------------------------- #
@pn.depends(w_stations)
def station_info(stations):
    if not stations:
        return pn.pane.Markdown("")
    cols = [c for c in ["ID", "station", "provider", "country", "elevation", "lon", "lat"] if c in P]
    return pn.widgets.Tabulator(P.loc[P.ID.isin(stations), cols], show_index=False, disabled=True,
                                height=min(40 + 32 * len(stations), 260))


# --------------------------------------------------------------------------- #
# Layout
# --------------------------------------------------------------------------- #
n_obs = int(DB.ts["value"].notna().sum())
header_md = (f"**{len(P)}** stations · **{len(VAR_ID)}** variables · **{n_obs:,}** valid values · "
             f"{TMIN:%Y-%m-%d} → {TMAX:%Y-%m-%d}")

btn_clear = pn.widgets.Button(name="Clear stations", button_type="light")
btn_clear.on_click(lambda e: setattr(w_stations, "value", []))

sidebar = [
    pn.pane.Markdown(header_md),
    w_variable, w_variable_desc, w_stations, btn_clear, w_provider, w_dates,
    pn.pane.Markdown("**Aggregation**"), w_agg, w_cover,
    pn.layout.Divider(), w_compare, w_attr, w_minyears,
    pn.layout.Divider(), download,
]

def _tab(obj):
    return pn.Column(obj, min_height=500, sizing_mode="stretch_width")


tabs = pn.Tabs(
    ("Map & series", pn.Column(pn.pane.HoloViews(station_map, linked_axes=False), station_info, ts_plot)),
    ("Compare variables", _tab(compare_plot)),
    ("Availability", _tab(availability_plot)),
    ("Climatology", _tab(climatology_plot)),
    ("Attributes", _tab(attribute_plot)),
    ("Table", _tab(pn.bind(table_view, w_variable, w_compare, w_stations, w_dates, w_agg, w_cover, w_attr))),
    ("Schema", _tab(schema_view())),
    dynamic=os.environ.get("STDB_DYNAMIC", "0") == "1",
)

template = pn.template.FastListTemplate(
    title="spatiotemporal · db explorer",
    sidebar=sidebar, main=[tabs], accent="#2b6f8f", sidebar_width=330,
    header_background="#1f4e66",
)

# preselect the first well-covered station so the app opens on something useful
_best = (DB.availability[DB.availability.variable_id == VAR_ID[default_var]]
         .groupby("p_id")["n"].sum().sort_values(ascending=False).index[:2].astype(int).tolist())
w_stations.value = _best

template.servable()
