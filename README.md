# koala-nowcast

Automated nowcast pipeline and interactive dashboard for **KOALA**
(*KOALitions-Analyse*) — German election polls turned into Monte-Carlo–based
probabilities for seat distributions, threshold crossings and coalition
majorities.

**Live site: <https://koala-lmu.github.io/koala-nowcast/>**

The statistical engine lives separately in
[`adibender/coalitions`](https://github.com/adibender/coalitions) and is consumed
here as a dependency. This repo is the pipeline around it: scraping, pooling,
simulation, storage and presentation.

Currently covered (one YAML config each, see [config/elections/](config/elections/)):

| Election | ID | Seats | Allocation |
| --- | --- | --- | --- |
| Bundestagswahl 2029 | `btw` | 630 | Sainte-Laguë (`sls`) |
| Abgeordnetenhauswahl Berlin 2026 | `ltw_be` | 130 | Hare-Niemeyer |
| Landtagswahl Mecklenburg-Vorpommern 2026 | `ltw_mv` | 71 | Hare-Niemeyer |
| Landtagswahl Sachsen-Anhalt 2026 | `ltw_st` | 83 | Hare-Niemeyer |

## Architecture

Computation is decoupled from presentation: everything is pre-computed in CI, and
the site itself is static files on GitHub Pages. Nothing is calculated in the
browser beyond drawing.

```
                 ┌──────────────── GitHub Actions: compute.yml (4×/day) ─────────────────┐
wahlrecht.de ──▶ │ scrape_polls.R ──▶ pool ──▶ calc_coalProbs.R (Dirichlet MC + seats)   │
                 └───────┬──────────────────────────────────────────────┬────────────────┘
                         │                                              │
                         ▼                                              ▼
              data/surveys/<id>/polls.json                   data/results/<id>/*.json
                         │                                              │
                         └──────────▶ Supabase Storage (S3) ◀───────────┘
                                       bucket: koala-data
                                              │
                 ┌──────────── GitHub Actions: deploy.yml (on compute success) ──────────┐
                 │ prepare_data.R ──▶ dashboard/data/<id>/*.json ──▶ quarto render       │
                 └──────────────────────────────────┬────────────────────────────────────┘
                                                    ▼
                                       GitHub Pages (Quarto + Observable JS)
```

State lives in the **Supabase bucket**, not in git — poll and result data are
gitignored. Each CI run pulls the bucket down, does incremental work, and syncs
back.

## Layout

```
koala-nowcast/
├── .github/workflows/
│   ├── compute.yml            # scrape + pool + simulate, 4×/day + manual
│   └── deploy.yml             # build & publish the dashboard to Pages
├── config/elections/          # one YAML per election — drives scraping and computing
│   └── btw.yml  ltw_be.yml  ltw_mv.yml  ltw_st.yml
├── scripts/
│   ├── scrape_polls.R         # scrape_election(): scrape, impute, pool, save
│   ├── scrape_btw.R           # Bundestag scraper across all wahlrecht.de institutes
│   ├── calc_coalProbs.R       # calc_coalProbs(): Monte Carlo → all result files
│   ├── calc_coalProbs_helpers.R  # calc_allCoalProbs(), derive_dynamic_coalitions()
│   ├── pending_configs.R      # which elections/dates still need computing
│   └── test_calc_allCoalProbs.R  # standalone test script for the majority logic
├── dashboard/
│   ├── index.qmd              # Quarto dashboard (Observable JS + Plot/D3)
│   ├── prepare_data.R         # results → slim, dashboard-shaped JSON
│   ├── coalition_density.R    # seat-share density curves per coalition
│   └── data/<id>/             # generated, gitignored — what the browser fetches
├── data/                      # generated, gitignored — mirror of the Supabase bucket
│   ├── surveys/<id>/polls.json
│   └── results/<id>/*.json
├── DESCRIPTION                # R dependencies (read by setup-r-dependencies in CI)
└── website/                   # unused placeholder from the original scaffold
```

## How it works

### 1. Scrape and pool — `scripts/scrape_polls.R`

`scrape_election(config_path)` runs per election config:

- Calls the scraper named in `scraper.function` (`scrape_btw` locally, or
  `coalitions::scrape_ltw` with the config's `url`/`ind_row_remove`), normalises
  pollster names, and collapses parties to the config's party set.
- **Incremental:** existing `polls.json` is read and only dates from 30 days
  before the newest known poll are re-scraped. Returns `TRUE` only if something
  actually changed.
- Poll dates where any `required: true` party is missing are dropped entirely.
- **Imputation for pooling only:** a party occasionally hidden inside *Sonstige*
  is filled in from the most recent earlier poll (preferring the same institute),
  with the imputed share subtracted from *Sonstige*. Raw rows written to
  `polls.json` stay untouched — imputation feeds the pooling step alone.
- **Pooling** via `coalitions::pool_surveys()` for every date a poll exists.
  Polls within `pooling.period_extended` days of the target date enter the
  window; those older than `pooling.period` days count at half weight, and only
  the newest poll per institute is used. The result is stored back into
  `polls.json` as rows with `pollster == "pooled"`.
- Only dates from the earliest new poll onward are re-pooled; those dates are
  written to `pending_dates.json` so the compute step knows exactly what to redo.

### 2. Simulate — `scripts/calc_coalProbs.R`

`calc_coalProbs(config_path, nsim = 10000, correction = 0.005, cores = 1)` runs
per election, for every pollster **and** the pooled series, for each pending date:

1. `coalitions::draw_from_posterior()` — `nsim` Dirichlet draws of vote shares.
2. *Sonstige* is dropped, then `coalitions::get_seats()` allocates seats with the
   config's `seat_allocation` function over `parliament.seats`.
3. `calc_allCoalProbs()` enumerates every party subset and marks a coalition as
   *possible* in a simulation only if it holds a majority **and** no smaller
   subset of it already does — so "CDU-SPD-Grüne" is not counted when "CDU-SPD"
   alone already has 50 %.
4. **Leadership variants:** a coalition listed with several party orderings (e.g.
   `cdu|spd` and `spd|cdu`) is counted per ordering, only when the first-named
   party is the strongest one in that draw.
5. Derived outputs: probability each party clears `parliament.hurdle`, and the
   `analyses.biggest_party` "who becomes strongest force" contests.

**Dynamic coalitions.** If a config has no `coalitions:` key (as `btw.yml` does
not), the displayed set is derived at compute time by
`derive_dynamic_coalitions()` from current pooled shares: every subset up to size
4 whose combined share clears 25 %, with one leadership ordering per member
polling at least half of the subset's strongest party. This keeps the exposed set
tracking actual polling instead of a hand-maintained snapshot.

**Incremental again.** Dates are taken from `pending_dates.json`; failing that,
from whatever is missing in the existing results. `pending_configs.R` independently
compares scraped dates against computed ones across *all* result files, which
catches half-finished uploads that the scraper's own bookkeeping would miss.

### 3. Prepare and render — `dashboard/`

`prepare_data.R` reshapes results into small, purpose-built files per election —
the browser never reads `data/results/` directly. It also drops the pooling
warm-up (the first `period_extended` days after `scraper.oldest_date`, where the
window reaches back past the oldest poll held) and computes seat-share density
curves via `coalition_density.R`.

Size matters here: `coalition_densities.json` for the Bundestagswahl is 49 MB
naïvely and 3 MB after nesting each curve into two arrays and rounding to four
significant digits. `index.qmd` reads these files lazily with `FileAttachment()`,
so only the selected election is ever fetched.

Dashboard pages: **Überblick** · **Sitzverteilung** · **Zeitverlauf** ·
**Umfragedaten** · **Methodik** · **Über das Projekt**, with an election selector
in the sidebar.

## Running locally

Requirements: R with the packages in [DESCRIPTION](DESCRIPTION) (`coalitions`,
`dplyr`, `tidyr`, `jsonlite`, `yaml`), plus [Quarto](https://quarto.org) for the
dashboard. Run everything **from the project root** — all paths are relative to it.

```sh
# 1. Scrape + pool one election (or loop over config/elections/*.yml)
Rscript -e 'source("scripts/scrape_polls.R"); scrape_election("config/elections/btw.yml")'

# 2. Simulate. nsim/cores are worth lowering while iterating.
Rscript -e '
  suppressPackageStartupMessages({ library(coalitions); library(dplyr); library(tidyr); library(yaml); library(jsonlite) })
  source("scripts/calc_coalProbs_helpers.R"); source("scripts/calc_coalProbs.R")
  calc_coalProbs("config/elections/btw.yml")'

# 3. Reshape for the dashboard (loops over all four elections, skipping missing ones)
Rscript dashboard/prepare_data.R

# 4. Render and view
quarto preview dashboard/index.qmd
```

A full from-scratch scrape + compute for the Bundestagswahl takes a while; step 1
alone is enough to inspect poll data.

If you render with `quarto render` instead, **serve the output over HTTP** —
opening the built `index.html` via `file://` shows a blank page, because browsers
block the OJS ES-module scripts under that scheme:

```sh
quarto render dashboard/index.qmd --output-dir _site
python3 -m http.server 8765 --directory dashboard/_site
```

Run the majority-logic tests with:

```sh
Rscript scripts/test_calc_allCoalProbs.R
```

## Adding an election

Add one file to `config/elections/`; both workflows pick up every `*.yml` there
automatically. Keys:

```yaml
id: ltw_xx                    # directory name under data/ and dashboard/data/ (ltw_ is stripped for the latter)
name: Landtagswahl … 2026
hashtag: "#ltwxx"             # metadata only; not read by any script yet

scraper:
  function: scrape_ltw        # or scrape_btw
  url: https://www.wahlrecht.de/umfragen/landtage/….htm
  ind_row_remove: [1, 2]      # header rows wahlrecht.de puts in the table
  oldest_date: 2024-07-28     # how far back a from-scratch scrape reaches

parliament:
  seats: 83
  majority: 42                # display value; the simulation derives majority from seat counts
  hurdle: 0.05
  seat_allocation: hare_niemeyer   # any allocation function in the coalitions package

pooling:
  period: 14                  # full-weight window
  period_extended: 100        # half-weight window; state polls are sparse

parties:                      # id / label / color; required: false = may be absent from a poll
  - { id: cdu, label: Union, color: "#000000", required: true }
  # …
  - { id: others, label: Sonstige, color: "#808080", required: true }

pollsters: [allensbach, forsa, insa]   # institutes eligible for pooling

# coalitions:                 # omit to derive dynamically from current pooled shares
analyses:
  biggest_party:
    - parties: [cdu, spd, afd]
      label: "Wer wird stärkste Kraft?"
  pass_hurdle:                # currently informational; passHurdle.json covers all parties
    parties: [fdp, bsw]
    label: "Schaffen es die folgenden Parteien in den Landtag?"
```

Set `oldest_date` at least `period_extended` days before the first date you want
shown — earlier dates pool over an incompletely scraped window and are hidden by
`prepare_data.R`.

Two things outside the config also need touching:

- `dashboard/prepare_data.R` — the election id list at the bottom of the file.
- `dashboard/index.qmd` — the `input-election` selector and the `election-files`
  cell (Quarto resolves `FileAttachment()` paths statically at render time, so
  they cannot be built from a variable), plus the `election_meta` block in
  `load-dashboard-data`. That block deliberately restates seats, majority,
  allocation method, hurdle and pooling window for the Methodik page and **must be
  kept in sync with the YAML by hand**. Party colors and display names on the
  dashboard come from that same block, not from the config's `color:`/`label:`
  fields — those feed the derived coalition metadata on the compute side.

## Data files

Generated per election under `data/`, mirrored to the `koala-data` Supabase bucket:

| File | Contents |
| --- | --- |
| `surveys/<id>/polls.json` | Raw polls plus the pooled series (`pollster == "pooled"`), one row per pollster/date/party |
| `surveys/<id>/pending_dates.json` | Transient: dates awaiting computation; deleted once consumed |
| `results/<id>/coalProbs.json` | Every enumerated coalition, per pollster/date: probability, size, log-odds. The largest file; only ever read back by the pipeline |
| `results/<id>/coalProbs_grouping.json` | Just the configured/derived coalitions with their labels — what the dashboard uses |
| `results/<id>/passHurdle.json` | Probability each party clears the threshold |
| `results/<id>/biggestParty.json` | "Strongest force" probabilities per configured contest |
| `results/<id>/shares.json` | Per-simulation seat shares (subsampled to 1000 draws), newest date per pollster only |

`prepare_data.R` turns those into `dashboard/data/<id>/`:
`coalition_probabilities.json`, `party_shares.json`, `poll_history.json`,
`coalition_history.json`, `hurdle_probabilities.json`, `per_pollster.json`,
`coalition_densities.json`. Each carries an `updated` field — the date of the
newest raw poll behind the numbers, not a build timestamp.

## CI/CD

| Workflow | Trigger | Does |
| --- | --- | --- |
| [compute.yml](.github/workflows/compute.yml) | `cron 0 2,8,14,20 * * *` (4/10/16/22 CEST) + manual | Pull bucket → scrape + pool → compute pending elections → sync back |
| [deploy.yml](.github/workflows/deploy.yml) | Successful `compute.yml` run + manual | Pull bucket (skipping `coalProbs.json`) → `prepare_data.R` → `quarto render` → GitHub Pages |

Compute only runs when the scrape found new polls, when `pending_configs.R`
reports scraped-but-uncomputed dates, or when the bucket is empty (`FORCE_ALL`).

Repository secrets required: `SUPABASE_S3_KEY_ID` and `SUPABASE_S3_SECRET`.
Both workflows use path-style S3 addressing and region `eu-central-1`, because
Supabase's endpoint carries a path (`/storage/v1/s3`) and signs against the
project's real region.

## Credits

Poll data from [wahlrecht.de](https://www.wahlrecht.de). Statistical methods from
the `coalitions` package — Bender & Bauer (2018),
[JOSS 3(23), 606](https://doi.org/10.21105/joss.00606).

## License

MIT — see [LICENSE](LICENSE).
