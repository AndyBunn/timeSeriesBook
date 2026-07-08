# Permissions evidence

This folder holds evidence of the licenses for third-party images used in the
book. The machine-readable log of all credits is `../permissions.csv`. This
folder is where the supporting snapshots go.

## Why snapshot

The one image currently logged (Allison Horst's ACF artwork) is openly
licensed (Creative Commons), so no permission request or fee is required. But
the general license statement lives on her site, not on the specific artwork
page, and a site can change after the fact. A dated snapshot taken now is your
proof of the terms in force when the image was used.

## What to capture

Save a PDF or full-page screenshot of both pages, showing the license
statement and the author's name:

| Save as | From |
|---|---|
| `host_acf_general-license.pdf` | <https://allisonhorst.com/allison-horst> (the CC BY 4.0 statement) |
| `host_acf_page.pdf` | <https://allisonhorst.com/time-series-acf> (the specific artwork) |

In a browser, "Print to PDF" of the full page is the simplest capture and
preserves the URL and date in the footer.

## When you add a new third-party image

1. Add a row to `../permissions.csv`.
2. Add a credit line in the figure caption (already done for the Horst
   artwork, see `04Autocorrelation.qmd`'s `auto-horst-jpg` chunk).
3. Add it to the Image Credits section in `A2DataSources.qmd`.
4. Snapshot its license page into this folder.
