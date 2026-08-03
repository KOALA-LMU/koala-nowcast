---
title: "KOALA"
subtitle: "Koalitionswahrscheinlichkeitsrechner LMU"
format:
  dashboard:
    theme: cosmo
    nav-buttons:
      - icon: github
        href: https://github.com/adibender/koala
---

```{=html}
<style>
.card-body,
.card-body .html-fill-item,
.card-body .cell,
.card-body .cell-output-display {
  justify-content: flex-start !important;
  align-content: flex-start !important;
}

.card-body {
  padding-top: 0.5rem !important;
}

.card-body figure > h2 {
  font-size: 1rem !important;
}

.card-body figure > h3 {
  font-size: 0.8rem !important;
}

/* Cards built from multiple ojs cells (via ::: {.card} fences) get one
   .cell.card-body wrapper per cell, stacked as flex siblings that each grow
   to an equal share of the card's height by default. Shrink every cell to
   its own content height instead, so leftover space collapses to the
   bottom of the card rather than sitting as a gap between cells. */
.zeitverlauf-card > .cell.card-body {
  flex: 0 0 auto !important;
}

/* The Methodik page is one long text card rather than a chart: let it scroll
   inside the card instead of being clipped on short viewports. */
.methodik-card > .cell.card-body {
  overflow-y: auto !important;
}

.card-body svg [aria-label="tick"] text {
  font-size: 13px;
}

.overview-coalitions svg [aria-label="y-axis tick"] text,
.overview-coalitions svg [aria-label="tick"] text {
  font-size: 14px;
}
</style>

<script>
// Plot writes its size into <svg width height> and emits no viewBox, so CSS alone
// can only crop a chart, never scale it — the fit rules in the stylesheet above do
// nothing until the viewBox exists. Adding one (0 0 width height, i.e. exactly the
// size Plot laid the chart out at) leaves the chart pixel-identical while it fits
// and scales it down proportionally once the card is too short.
//
// The crosshair overlays keep working on a scaled chart: they position via
// plot.scale(...) in user-space coordinates, which the viewBox maps, and read the
// pointer via d3.pointer(event, svg), which inverts the live screen CTM.
//
// OJS renders cells asynchronously and re-renders them whenever an input changes,
// so charts appear long after DOMContentLoaded — hence an observer instead of a
// one-shot pass. Elements that already carry a viewBox (Bootstrap icons) are left
// untouched.
(() => {
  const addViewBox = svg => {
    if (svg.hasAttribute("viewBox")) return
    const w = svg.getAttribute("width")
    const h = svg.getAttribute("height")
    if (!w || !h) return
    svg.setAttribute("viewBox", `0 0 ${w} ${h}`)
  }

  const scan = node => {
    if (node.nodeType !== Node.ELEMENT_NODE) return
    if (node.matches("svg[width][height]")) addViewBox(node)
    node.querySelectorAll("svg[width][height]").forEach(addViewBox)
  }

  new MutationObserver(records => {
    for (const record of records) record.addedNodes.forEach(scan)
  }).observe(document.documentElement, { childList: true, subtree: true })

  document.addEventListener("DOMContentLoaded", () => scan(document.body))
})()
</script>
```


::: {.cell}

:::










































