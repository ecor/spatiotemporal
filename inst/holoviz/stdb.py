"""
stdb.py -- Python access layer for the `spatiotemporal` R package database.

The R object `db` (see `?spatiotemporal::db`) is a named list of tables:

    operator, project, provider, action, p (sf POINT), variable, ts, attribute

with foreign keys named `<table>_id` pointing to `<table>$ID`.

Reading an .rda/.rds with the pure-Python `rdata` package is slow for large
`ts` tables (minutes), so the first load converts the database to a folder of
Parquet files and later loads read the Parquet cache (seconds).

Usage (command line):

    python stdb.py ../../data/db.rda                  # -> ../../data/db_parquet/
    python stdb.py db_icpac_v2.rds -o /tmp/icpac_pq   # custom output folder
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
import warnings
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np
import pandas as pd

TABLES = ["operator", "project", "provider", "action", "p", "variable", "ts", "attribute"]


# --------------------------------------------------------------------------- #
# Conversion R -> Parquet
# --------------------------------------------------------------------------- #
def _read_r_db(path: Path, object_name: str | None = None) -> dict:
    """Read an R database (named list) from .rda/.RData or .rds."""
    import rdata  # imported lazily: not needed once the Parquet cache exists

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        if path.suffix.lower() == ".rds":
            obj = rdata.read_rds(str(path), default_encoding="utf8")
        else:
            env = rdata.read_rda(str(path), default_encoding="utf8")
            if object_name is None:
                object_name = "db" if "db" in env else next(iter(env))
            obj = env[object_name]
    return {str(k): v for k, v in obj.items()}


def _posix_to_datetime(s: pd.Series) -> pd.Series:
    # R POSIXct is seconds since epoch (UTC)
    return pd.to_datetime(pd.to_numeric(s, errors="coerce"), unit="s", utc=True).dt.tz_localize(None)


def _fix_table(name: str, df: pd.DataFrame) -> pd.DataFrame:
    df = df.reset_index(drop=True).copy()
    for c in df.columns:
        if c == "timestamptz":
            df[c] = _posix_to_datetime(df[c])
        elif c.endswith("_id") or c == "ID":
            df[c] = pd.to_numeric(df[c], errors="coerce").astype("Int32")
        elif str(df[c].dtype).startswith("string"):
            df[c] = df[c].astype("string")
    if name == "p" and "geometry" in df.columns:
        xy = np.array([np.asarray(g, dtype=float)[:2] if g is not None else [np.nan, np.nan]
                       for g in df["geometry"]])
        df["lon"], df["lat"] = xy[:, 0], xy[:, 1]
        df = df.drop(columns="geometry")
    if name == "ts":
        if "attrvalue" in df.columns:
            df["attrvalue"] = df["attrvalue"].astype("category")
        df = df.sort_values(["variable_id", "p_id", "timestamptz"], kind="stable").reset_index(drop=True)
    return df


def convert(src: str | os.PathLike, out_dir: str | os.PathLike | None = None,
            object_name: str | None = None, verbose: bool = True) -> Path:
    """Convert an R `db` (.rda/.rds) into a folder of Parquet files."""
    src = Path(src)
    out_dir = Path(out_dir) if out_dir else src.with_name(src.stem + "_parquet")
    out_dir.mkdir(parents=True, exist_ok=True)
    t0 = time.time()
    if verbose:
        print(f"[stdb] reading {src} (pure-Python R reader, may take a while)...", flush=True)
    raw = _read_r_db(src, object_name)
    meta = {"source": str(src.resolve()), "source_mtime": src.stat().st_mtime, "tables": {}}
    for name, df in raw.items():
        if not isinstance(df, pd.DataFrame):
            continue
        df = _fix_table(name, df)
        df.to_parquet(out_dir / f"{name}.parquet", index=False)
        meta["tables"][name] = {"rows": int(len(df)), "columns": {c: str(t) for c, t in df.dtypes.items()}}
        if verbose:
            print(f"[stdb]   {name:<10} {len(df):>10,} rows", flush=True)
    (out_dir / "_meta.json").write_text(json.dumps(meta, indent=2))
    if verbose:
        print(f"[stdb] done in {time.time() - t0:.1f}s -> {out_dir}", flush=True)
    return out_dir


# --------------------------------------------------------------------------- #
# Loading
# --------------------------------------------------------------------------- #
_PRCP_ATTR = {
    "A": "1 report of 6-hour precipitation amount",
    "B": "Summation of 2 reports of 6-hour precipitation amount",
    "C": "Summation of 3 reports of 6-hour precipitation amount",
    "D": "Summation of 4 reports of 6-hour precipitation amount",
    "E": "1 report of 12-hour precipitation amount",
    "F": "Summation of 2 reports of 12-hour precipitation amount",
    "G": "1 report of 24-hour precipitation amount",
    "H": "Reported 0 but precipitation occurred in hourly obs (incomplete)",
    "I": "No precipitation data and no occurrence reported",
}


@dataclass
class SpatioTemporalDB:
    tables: dict[str, pd.DataFrame]
    source: str = ""
    # derived
    availability: pd.DataFrame = field(default=None, repr=False)  # variable_id, p_id, year, n

    # ---- convenience accessors ------------------------------------------- #
    def __getitem__(self, k: str) -> pd.DataFrame:
        return self.tables[k]

    @property
    def p(self) -> pd.DataFrame:
        return self.tables["p"]

    @property
    def ts(self) -> pd.DataFrame:
        return self.tables["ts"]

    @property
    def variable(self) -> pd.DataFrame:
        return self.tables["variable"]

    @property
    def provider(self) -> pd.DataFrame:
        return self.tables["provider"]

    def station_label(self) -> dict[int, str]:
        p = self.p
        col = "name2" if "name2" in p.columns else ("name" if "name" in p.columns else "ID")
        return dict(zip(p["ID"].astype(int), p[col].astype(str)))

    def variable_label(self) -> dict[int, str]:
        v = self.variable
        return dict(zip(v["ID"].astype(int), v["name"].astype(str)))

    def attr_legend(self, variable_name: str) -> dict[str, str]:
        return _PRCP_ATTR if variable_name.upper() == "PRCP" else {}

    # ---- R-like getters (mirror get_p / get_variable / get_ts) ---------- #
    def get_p(self, provider_id=None, bbox=None) -> pd.DataFrame:
        p = self.p
        if provider_id:
            p = p[p["provider_id"].isin(list(provider_id))]
        if bbox is not None:
            x0, y0, x1, y1 = bbox
            p = p[p.lon.between(x0, x1) & p.lat.between(y0, y1)]
        return p

    def get_ts(self, p_id=None, variable_id=None, start=None, end=None,
               attrvalue=None, dropna=True) -> pd.DataFrame:
        ts = self.ts
        m = np.ones(len(ts), dtype=bool)
        if variable_id is not None:
            m &= ts["variable_id"].isin(list(variable_id)).to_numpy()
        if p_id is not None:
            m &= ts["p_id"].isin(list(p_id)).to_numpy()
        if start is not None:
            m &= (ts["timestamptz"] >= pd.Timestamp(start)).to_numpy()
        if end is not None:
            m &= (ts["timestamptz"] <= pd.Timestamp(end)).to_numpy()
        if attrvalue:
            m &= ts["attrvalue"].isin(list(attrvalue)).to_numpy()
        out = ts.loc[m]
        if dropna:
            out = out[out["value"].notna()]
        return out

    def schema(self) -> list[dict]:
        """Columns, dtypes and foreign keys (same rule as the R README)."""
        names = set(self.tables)
        out = []
        for t, df in self.tables.items():
            cols = []
            for c, dt in df.dtypes.items():
                ref = c[:-3] if c.endswith("_id") and c[:-3] in names else None
                cols.append({"column": c, "type": str(dt), "references": ref})
            out.append({"table": t, "rows": len(df), "columns": cols})
        return out


def _compute_availability(ts: pd.DataFrame) -> pd.DataFrame:
    ok = ts[ts["value"].notna()]
    return (ok.assign(year=ok["timestamptz"].dt.year.astype("int16"))
              .groupby(["variable_id", "p_id", "year"], observed=True).size()
              .rename("n").reset_index())


def load(path: str | os.PathLike, rebuild: bool = False) -> SpatioTemporalDB:
    """Load a database from a Parquet folder, or from .rda/.rds (cached as Parquet)."""
    path = Path(path).expanduser()
    if path.is_file():
        cache = path.with_name(path.stem + "_parquet")
        stale = True
        if (cache / "_meta.json").exists() and not rebuild:
            meta = json.loads((cache / "_meta.json").read_text())
            stale = meta.get("source_mtime", 0) < path.stat().st_mtime
        if stale:
            convert(path, cache)
        folder = cache
    else:
        folder = path
    tables = {}
    for f in sorted(folder.glob("*.parquet")):
        tables[f.stem] = pd.read_parquet(f)
    missing = {"p", "ts", "variable"} - set(tables)
    if missing:
        raise FileNotFoundError(f"{folder}: missing tables {sorted(missing)}")
    for t, df in tables.items():
        if "attrvalue" in df.columns:
            df["attrvalue"] = df["attrvalue"].astype("category")
        # Parquet written from R (arrow) stores POSIXct as tz-aware UTC: make naive UTC
        if "timestamptz" in df.columns and getattr(df["timestamptz"].dt, "tz", None) is not None:
            df["timestamptz"] = df["timestamptz"].dt.tz_convert("UTC").dt.tz_localize(None)
        if t == "p" and not {"lon", "lat"} <= set(df.columns):
            raise ValueError("table p needs lon/lat columns (see export_db_parquet.R)")
    db = SpatioTemporalDB(tables=tables, source=str(path))
    db.availability = _compute_availability(tables["ts"])
    return db


def _parse_description(desc: str) -> dict:
    """Parse 'KEY : value , KEY : value' strings used in p$description (GSOD)."""
    out = {}
    for part in re.split(r"\s,\s", str(desc)):
        if " : " in part:
            k, v = part.split(" : ", 1)
            out[k.strip()] = v.strip()
    return out


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Convert a spatiotemporal R db (.rda/.rds) to Parquet")
    ap.add_argument("src")
    ap.add_argument("-o", "--out", default=None)
    ap.add_argument("--object", default=None, help="object name inside the .rda (default: db)")
    a = ap.parse_args()
    convert(a.src, a.out, a.object)
    sys.exit(0)
