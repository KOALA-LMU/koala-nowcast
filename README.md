# koala-nowcast

[![Live dashboard](https://img.shields.io/badge/Live_dashboard-koala--lmu.github.io-46962b?style=for-the-badge)](https://koala-lmu.github.io/koala-nowcast/)

Automated nowcast pipeline and interactive dashboard for **KOALA**
(*KOALitions-Analyse*, LMU München) — German election polls turned into
Monte-Carlo-based probabilities for seat distributions, threshold crossings and
coalition majorities.

👉 **<https://koala-lmu.github.io/koala-nowcast/>** — updated four times a day
from the latest wahlrecht.de polls.

The statistical engine lives separately in
[`adibender/coalitions`](https://github.com/adibender/coalitions) and is consumed
here as a dependency.

## Elections covered

| Config | Election | Seats | Seat allocation |
| --- | --- | --- | --- |
| `config/elections/btw.yml` | Bundestagswahl 2029 | 630 | Sainte-Laguë/Schepers |
| `config/elections/ltw_st.yml` | Landtagswahl Sachsen-Anhalt 2026 | 83 | Hare-Niemeyer |
| `config/elections/ltw_mv.yml` | Landtagswahl Mecklenburg-Vorpommern 2026 | 71 | Hare-Niemeyer |
| `config/elections/ltw_be.yml` | Abgeordnetenhauswahl Berlin 2026 | 130 | Hare-Niemeyer |

Adding an election means adding one YAML file — see
[Adding an election](#adding-an-election).

## How it works

Computation is decoupled from presentation: everything is pre-computed on a
schedule, and the site is a static bundle of JSON plus Observable JS.

```
wahlrecht.de
     │  scrape_polls.R          scrape → pool (Bayesian, 14/28-day window)
     ▼
data/surveys/<id>/polls.json ─────────────────────────┐
     │  calc_coalProbs.R                              │
     │  Dirichlet posterior → seat allocation →       │  Supabase Storage
     │  coalition majorities                          │  (S3-compatible,
     ▼                                                │   holds pipeline state
data/results/<id>/*.json ─────────────────────────────┘   between runs)
     │  dashboard/prepare_data.R   reshape + downsample for the browser
     ▼
dashboard/data/<id>/*.json
     │  quarto render
     ▼
GitHub Pages
```

Raw and computed data are **not** committed to the repo (see `.gitignore`) —
`data/results/btw` alone is ~160 MB. Supabase Storage is the source of truth
between runs; both workflows sync from it on start.

### 1. Scrape and pool — [`scripts/scrape_polls.R`](scripts/scrape_polls.R)

`scrape_election(config_path)` scrapes wahlrecht.de via `coalitions::scrape_wahlrecht()`
(federal polls through [`scripts/scrape_btw.R`](scripts/scrape_btw.R), state polls
through `coalitions::scrape_ltw()`), then:

- normalises pollster names and collapses parties to the config's party set;
- drops poll dates where any `required: true` party is missing;
- **incremental by default** — only scrapes from 30 days before the newest
  existing poll, and only re-pools dates from the earliest new poll forward;
- imputes missing parties from the most recent prior poll (preferring the same
  pollster) *for the pooling step only* — raw rows in `polls.json` stay untouched;
- writes pooled estimates as a synthetic `pollster == "pooled"` series, plus
  `pending_dates.json` telling the compute step exactly which dates changed.

Pooling uses `coalitions::pool_surveys()` over a `period`-day window, widening to
`period_extended` days when the short window is empty — important for state
elections, which are polled infrequently.

### 2. Compute — [`scripts/calc_coalProbs.R`](scripts/calc_coalProbs.R)

`calc_coalProbs(config_path)` runs, per pollster and per date:

1. `coalitions::draw_from_posterior()` — 10,000 draws from the Dirichlet
   posterior over vote shares (`correction = 0.005`);
2. `coalitions::get_seats()` — seat allocation per draw using the config's method
   (`sls` or `hare_niemeyer`), with `others` excluded before allocation;
3. [`calc_allCoalProbs()`](scripts/calc_coalProbs_helpers.R) — for every party
   combination, the share of draws where it holds a majority **and no subset of
   it already does**, so a majority is attributed to the smallest coalition that
   achieves it;
4. `biggestParty` and `passHurdle` probabilities from the `analyses:` block.

Coalitions are **derived dynamically** from current pooled vote shares rather
than hand-listed: `derive_dynamic_coalitions()` enumerates combinations up to
four parties that clear 25% combined, and generates one variant per plausible
leading party (any party within 50% of the combination's strongest). Setting an
explicit `coalitions:` key in the YAML overrides this.

Only dates in `pending_dates.json` are recomputed; results for other dates are
read back and merged. Output is downsampled to 1,000 retained simulations and
rounded to four significant digits before writing.

### 3. Prepare and render — [`dashboard/prepare_data.R`](dashboard/prepare_data.R), [`dashboard/index.qmd`](dashboard/index.qmd)

`prepare_data.R` reshapes results into the seven small JSON files the browser
loads per election, including pre-computing coalition seat-share **densities**
(kernel density with `bw = "bcv"`, AfD coalitions excluded) and nesting the
curves — for the Bundestagswahl this takes that file from 49 MB to 3 MB.

The Quarto dashboard has six pages: *Überblick*, *Koalitionswahrscheinlichkeiten*,
*Zeitverlauf*, *Umfragedaten*, *Methodik*, *Über das Projekt*. Data is fetched by
the browser from `data/` rather than inlined through `ojs_define()`, so
`index.html` stays small.

## Repository layout

```
koala-nowcast/
├── .github/workflows/
│   ├── compute.yml           # scrape → pool → compute, 4× daily
│   └── deploy.yml            # prepare → render → GitHub Pages
├── config/elections/*.yml    # one file per election — the only place to configure
├── scripts/
│   ├── scrape_btw.R          # federal scraper over coalitions' pollster table
│   ├── scrape_polls.R        # scrape_election(), imputation, pooling
│   ├── calc_coalProbs.R      # main computation entry point
│   ├── calc_coalProbs_helpers.R
│   ├── pending_configs.R     # which elections still need computing
│   └── test_calc_allCoalProbs.R
├── dashboard/
│   ├── prepare_data.R        # results → browser-sized JSON
│   ├── index.qmd             # Quarto + Observable JS dashboard
│   └── data/<id>/*.json      # generated, gitignored
├── data/                     # generated, gitignored
│   ├── surveys/<id>/polls.json
│   └── results/<id>/*.json
└── DESCRIPTION               # R dependencies
```

## Data files

**`data/surveys/<id>/`** — one flat record per (pollster, date, party):

| File | Contents |
| --- | --- |
| `polls.json` | Raw scraped polls plus the `pooled` series |
| `pending_dates.json` | Dates awaiting computation (removed once consumed) |

**`data/results/<id>/`**:

| File | Contents |
| --- | --- |
| `coalProbs.json` | Every coalition's majority probability, all dates (large; compute-side only) |
| `coalProbs_grouping.json` | Probabilities per labelled coalition — what the dashboard reads |
| `shares.json` | Per-simulation seat shares, newest date per pollster only |
| `passHurdle.json` | Probability each party clears the 5% threshold |
| `biggestParty.json` | Probability each party finishes strongest |

**`dashboard/data/<id>/`** — `coalition_probabilities`, `party_shares`,
`poll_history`, `coalition_history`, `hurdle_probabilities`, `per_pollster`,
`coalition_densities`.

## Running locally

Requires R and [Quarto](https://quarto.org). Install dependencies from
`DESCRIPTION`:

```r
install.packages("remotes")
remotes::install_deps(dependencies = TRUE)
remotes::install_github("adibender/coalitions")
```

All scripts assume the **project root** as the working directory.

```sh
# 1. Scrape and pool (writes data/surveys/<id>/polls.json)
Rscript -e 'source("scripts/scrape_polls.R"); scrape_election("config/elections/ltw_be.yml")'

# 2. Compute (writes data/results/<id>/)
Rscript -e '
  source("scripts/calc_coalProbs_helpers.R"); source("scripts/calc_coalProbs.R")
  calc_coalProbs("config/elections/ltw_be.yml")
'

# 3. Prepare dashboard data (all four elections; skips any without results)
Rscript dashboard/prepare_data.R

# 4. Render and serve
quarto render dashboard/index.qmd
python3 -m http.server 8765 --directory dashboard/_site
```

Open <http://localhost:8765>. Serving over HTTP is required — opening the built
`index.html` via `file://` shows a blank page, because browsers block the
Observable JS ES-module scripts and the `fetch()` calls for `data/`.

Useful arguments: `calc_coalProbs(..., cores = 4)` parallelises the coalition
enumeration, and `force_newCalculation = TRUE` recomputes every date instead of
just the pending ones. Start with a state election — the Bundestagswahl has far
more polling history and takes correspondingly longer.

Unit tests for the majority logic:

```sh
Rscript scripts/test_calc_allCoalProbs.R
```

## Automation

**`Scrape, Pool and Compute`** ([compute.yml](.github/workflows/compute.yml)) —
runs at 04:00, 10:00, 16:00 and 22:00 CEST, and on manual dispatch. It syncs
`data/` down from Supabase, scrapes every config, computes what is pending, and
syncs back up.

Because surveys are uploaded before results, a run that scrapes successfully but
then fails can leave polls in the bucket with no results behind them — and the
next run would see those polls as "already scraped" and skip computing forever.
[`pending_configs.R`](scripts/pending_configs.R) closes that gap independently of
what the scrape step reported, by comparing scraped dates against the dates
present in *every* result file. It scans `shares.json` with a regex rather than
parsing it, which turns a multi-minute `fromJSON()` into about a second.

**`Deploy Website`** ([deploy.yml](.github/workflows/deploy.yml)) — triggered on
successful completion of the compute workflow (or manually). Pulls data from
Supabase (skipping the large `coalProbs.json`, which the dashboard never reads),
prepares, renders and publishes to GitHub Pages.

Both need the repository secrets `SUPABASE_S3_KEY_ID` and `SUPABASE_S3_SECRET`.
Supabase's S3 endpoint carries a path prefix, so both workflows set path-style
addressing and sign against `eu-central-1` — `auto` fails.

## Adding an election

Drop a new YAML into `config/elections/`; both workflows pick up every `.yml`
there automatically. The one manual step is
[`dashboard/prepare_data.R`](dashboard/prepare_data.R), whose election list is
explicit, and the `election_meta` block in
[`dashboard/index.qmd`](dashboard/index.qmd), which carries the display strings
and election dates.

```yaml
id: ltw_xy                    # directory name under data/
name: Landtagswahl Musterland 2027
hashtag: "#ltwxy"

scraper:
  function: scrape_ltw        # or scrape_btw for the federal election
  url: https://www.wahlrecht.de/umfragen/landtage/musterland.htm
  ind_row_remove: [1, 2]      # header rows to drop
  oldest_date: 2024-07-28     # far enough back that the first shown date has a full pooling window

parliament:
  seats: 100
  majority: 51
  hurdle: 0.05
  seat_allocation: hare_niemeyer   # or sls

pooling:
  period: 14                  # primary window in days
  period_extended: 100        # fallback when the primary window is empty

parties:                      # id, label, color; `required` gates poll completeness
  - { id: cdu, label: Union, color: "#000000", required: true }
  # ...
  - { id: others, label: Sonstige, color: "#808080", required: false }

pollsters: [allensbach, civey, emnid, forsa, fgw, gms, infratest, insa]

# coalitions:                 # omit to derive dynamically from pooled shares

analyses:
  biggest_party:
    - parties: [cdu, spd, greens, left, afd]
      label: "Wer wird stärkste Kraft?"
  pass_hurdle:
    parties: [fdp, bsw]
    label: "Schaffen es die folgenden Parteien in den Landtag?"
```

Party colours and labels also appear in `party_colors` / `party_names` in
`index.qmd`; a party new to the project needs adding there too.

## Dependencies

`coalitions` (≥ 0.6.27), `dplyr` (≥ 1.2.1), `jsonlite` (≥ 2.0.0), `tidyr`,
`yaml` (≥ 2.3.10) — see [DESCRIPTION](DESCRIPTION). The dashboard additionally
needs Quarto, `knitr` and `rmarkdown`.

## License

MIT — see [LICENSE](LICENSE).
