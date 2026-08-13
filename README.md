# koala-nowcast

Automated nowcast pipeline and dashboard for **KOALA** (*KOALitions-Analyse*) —
German election polls turned into Monte-Carlo–based probabilities for seat
distributions, threshold crossings and coalition majorities.

**Live site: <https://koala-lmu.github.io/koala-nowcast/>**

The statistical engine is [`adibender/coalitions`](https://github.com/adibender/coalitions),
consumed as a dependency; this repo is the pipeline around it. One YAML config
per election in [config/elections/](config/elections/):

| Election | ID | Seats | Allocation |
| --- | --- | --- | --- |
| Bundestagswahl 2029 | `btw` | 630 | Sainte-Laguë (`sls`) |
| Abgeordnetenhauswahl Berlin 2026 | `ltw_be` | 130 | Hare-Niemeyer |
| Landtagswahl Mecklenburg-Vorpommern 2026 | `ltw_mv` | 71 | Hare-Niemeyer |
| Landtagswahl Sachsen-Anhalt 2026 | `ltw_st` | 83 | Hare-Niemeyer |

## Architecture

Everything is pre-computed in CI; the site is static files on GitHub Pages.
Nothing is calculated in the browser beyond drawing.

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

State lives in the **Supabase bucket**, not in git — `data/` and
`dashboard/data/` are generated and gitignored. Each CI run pulls the bucket
down, does incremental work, syncs back. `scripts/` is the pipeline,
`dashboard/` the frontend, `website/` an unused placeholder; R dependencies are
in [DESCRIPTION](DESCRIPTION).

## How it works

**1. Scrape and pool** — `scrape_election(config_path)` in
[scripts/scrape_polls.R](scripts/scrape_polls.R). Scrapes via
`scraper.function`, normalises pollsters, collapses parties to the config's set.
Only dates from 30 days before the newest known poll are re-scraped, and dates
missing a `required: true` party are dropped. A party hidden inside *Sonstige*
is imputed from the most recent earlier poll (preferring the same institute) and
subtracted from *Sonstige* — for pooling only; `polls.json` keeps raw rows.
`pool_surveys()` then weights polls within `pooling.period` fully and those out
to `period_extended` at half, newest poll per institute only, writing the result
back as `pollster == "pooled"` rows. Re-pooled dates go to `pending_dates.json`.

**2. Simulate** — `calc_coalProbs(config_path, nsim = 10000, ...)` in
[scripts/calc_coalProbs.R](scripts/calc_coalProbs.R), per pollster and pooled
series, per pending date: `draw_from_posterior()` draws vote shares, *Sonstige*
is dropped, `get_seats()` allocates via `seat_allocation`. A coalition counts as
*possible* only if it holds a majority **and** no smaller subset of it already
does — "CDU-SPD-Grüne" doesn't count when "CDU-SPD" alone has 50 %. Coalitions
listed in several orderings (`cdu|spd`, `spd|cdu`) count per ordering, only when
the first-named party is strongest in that draw. With no `coalitions:` key (as
in `btw.yml`) the set is derived from current pooled shares: subsets up to size 4
clearing 25 % combined, one ordering per member polling at least half of the
subset's strongest party. Pending dates come from `pending_dates.json`, failing
that from [scripts/pending_configs.R](scripts/pending_configs.R), which compares
scraped against computed dates across all result files and so catches
half-finished uploads.

**3. Prepare and render** — [dashboard/prepare_data.R](dashboard/prepare_data.R)
reshapes results into small per-election files; the browser never reads
`data/results/`. It hides the pooling warm-up (the first `period_extended` days
after `scraper.oldest_date`, where the window reaches past the oldest poll held)
and computes density curves via `coalition_density.R`. Size matters:
`coalition_densities.json` for the Bundestagswahl is 49 MB naïvely, 3 MB after
nesting each curve into two arrays and rounding to four significant digits.
`index.qmd` fetches lazily with `FileAttachment()`, so only the selected
election loads.

## Running locally

R with the packages in [DESCRIPTION](DESCRIPTION), plus
[Quarto](https://quarto.org). Run **from the project root** — paths are relative
to it.

```sh
# 1. Scrape + pool one election (or loop over config/elections/*.yml)
Rscript -e 'source("scripts/scrape_polls.R"); scrape_election("config/elections/btw.yml")'

# 2. Simulate. nsim/cores are worth lowering while iterating.
Rscript -e '
  suppressPackageStartupMessages({ library(coalitions); library(dplyr); library(tidyr); library(yaml); library(jsonlite) })
  source("scripts/calc_coalProbs_helpers.R"); source("scripts/calc_coalProbs.R")
  calc_coalProbs("config/elections/btw.yml")'

# 3. Reshape for the dashboard (all four elections, skipping missing ones)
Rscript dashboard/prepare_data.R

# 4. Render and view
quarto preview dashboard/index.qmd

# Majority-logic tests
Rscript scripts/test_calc_allCoalProbs.R
```

Step 1 alone is enough to inspect poll data; a full from-scratch run for the
Bundestagswahl takes a while. If you use `quarto render` instead of `preview`,
**serve over HTTP** — opening the built `index.html` via `file://` shows a blank
page, because browsers block the OJS ES-module scripts under that scheme:

```sh
quarto render dashboard/index.qmd --output-dir _site
python3 -m http.server 8765 --directory dashboard/_site
```

## Adding an election

Add one file to `config/elections/`; both workflows pick up every `*.yml` there.
See an existing config for the full shape — the keys that need thought:

| Key | Note |
| --- | --- |
| `id` | Directory under `data/` and `dashboard/data/` (`ltw_` stripped for the latter) |
| `scraper.function` | `scrape_ltw` (with `url`, `ind_row_remove`) or `scrape_btw` |
| `scraper.oldest_date` | Set ≥ `period_extended` days before the first date you want shown — earlier dates pool over an incompletely scraped window and are hidden |
| `parliament.majority` | Display value only; the simulation derives majority from seat counts |
| `pooling.period` / `period_extended` | Full-weight and half-weight windows; state polls are sparse, hence 14 / 100 |
| `parties[].required` | `false` = may be absent from a poll without dropping the date |
| `coalitions` | Omit to derive dynamically from current pooled shares |

Two things outside the config also need touching:

- `dashboard/prepare_data.R` — the election id list at the bottom.
- `dashboard/index.qmd` — the `input-election` selector and `election-files` cell
  (Quarto resolves `FileAttachment()` paths statically, so they can't come from a
  variable), plus `election_meta` in `load-dashboard-data`. That block restates
  seats, majority, allocation, hurdle and pooling window for the Methodik page
  and **must be kept in sync with the YAML by hand**. Party colors and display
  names come from there too, not from the config's `color:`/`label:`.

## Data files

Per election under `data/`, mirrored to the `koala-data` bucket:

| File | Contents |
| --- | --- |
| `surveys/<id>/polls.json` | Raw polls plus the pooled series, one row per pollster/date/party |
| `surveys/<id>/pending_dates.json` | Transient: dates awaiting computation |
| `results/<id>/coalProbs.json` | Every enumerated coalition per pollster/date. Largest file; only read back by the pipeline |
| `results/<id>/coalProbs_grouping.json` | Just the configured/derived coalitions with labels — what the dashboard uses |
| `results/<id>/passHurdle.json` | Probability each party clears the threshold |
| `results/<id>/biggestParty.json` | "Strongest force" probabilities per contest |
| `results/<id>/shares.json` | Per-simulation seat shares (1000 draws), newest date per pollster |

`prepare_data.R` turns those into seven slim files under `dashboard/data/<id>/`,
each carrying an `updated` field — the date of the newest raw poll behind the
numbers, not a build timestamp.

## CI/CD

| Workflow | Trigger | Does |
| --- | --- | --- |
| [compute.yml](.github/workflows/compute.yml) | `cron 0 2,8,14,20 * * *` (4/10/16/22 CEST) + manual | Pull bucket → scrape + pool → compute pending → sync back |
| [deploy.yml](.github/workflows/deploy.yml) | Successful `compute.yml` + manual | Pull bucket (skipping `coalProbs.json`) → `prepare_data.R` → `quarto render` → Pages |

Compute only runs when the scrape found new polls, when `pending_configs.R`
reports uncomputed dates, or when the bucket is empty (`FORCE_ALL`). Secrets:
`SUPABASE_S3_KEY_ID`, `SUPABASE_S3_SECRET`. Both workflows use path-style S3
addressing and region `eu-central-1`, because Supabase's endpoint carries a path
(`/storage/v1/s3`) and signs against the project's real region.

## Credits

Poll data from [wahlrecht.de](https://www.wahlrecht.de). Methods from the
`coalitions` package.

**Methodology publications**

- Bender, A. & Bauer, A. (2018). *coalitions: Coalition probabilities in
  multi-party democracies*. Journal of Open Source Software, 3(23), 606.
  <https://doi.org/10.21105/joss.00606>
- Bauer, A., Bender, A., Klima, A. et al. (2020). *KOALA: a new paradigm for
  election coverage*. AStA Advances in Statistical Analysis, 104, 101–115.
  <https://doi.org/10.1007/s10182-019-00352-6>
- Bauer, A., Klima, A., Gauß, J., Kümpel, H., Bender, A. & Küchenhoff, H. (2022).
  *Mundus Vult Decipi, Ergo Decipiatur: Visual Communication of Uncertainty in
  Election Polls*. PS: Political Science & Politics, 55(1), 102–108.
  <https://doi.org/10.1017/S1049096521000950>

MIT licensed — see [LICENSE](LICENSE).
