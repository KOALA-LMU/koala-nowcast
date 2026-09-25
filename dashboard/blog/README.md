# Election blog workflow

Each election post lives in its own `YYYYMM_<election>` directory and contains:

- `index.qmd`: article source
- `prepare_data.R`: election-specific data preparation and chart generation
- `index.html`: rendered article (generated)
- `election_result.png`, `diff_vs_results.png`, and `mae_by_pollster.png` (generated)

From the repository root, refresh the poll data before preparing a post:

```sh
Rscript -e 'source("scripts/scrape_polls.R"); scrape_election("config/elections/ltw_mv.yml")'
Rscript -e 'source("scripts/scrape_polls.R"); scrape_election("config/elections/ltw_be.yml")'
```

Then prepare and render each post:

```sh
Rscript dashboard/blog/202609_ltwmv/prepare_data.R
quarto render dashboard/blog/202609_ltwmv/index.qmd

Rscript dashboard/blog/202609_ltwbe/prepare_data.R
quarto render dashboard/blog/202609_ltwbe/index.qmd
```

The preparation scripts validate the final pooled estimate and institute dates. Add
the finished posts to the blog list in `dashboard/index.qmd` only after the article
text and generated figures have been reviewed.
