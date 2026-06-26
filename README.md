# Time Series Analysis for Environmental Science

*Course Notes*

**Read it here: [timeseries.andybunn.org](https://timeseries.andybunn.org/)**

<!-- Zenodo DOI badge goes here once the concept DOI is minted (Phase E):
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)
-->

This started as weekly handouts for ESCI 504, a graduate course in time series analysis at Western Washington University, and grew into a short book. It's free to read online and openly licensed.

## What it covers

The book works through time series analysis from the ground up, in four parts:

- **Preamble** - what a time series is, how R represents temporal data, and the vocabulary we'll use throughout
- **Decomposition & Autocorrelation** - trend and seasonal decomposition, the ACF and PACF, and ARMA models for stationary series
- **Forecasting & Cross-Correlation** - predicting from a fitted model and being honest about the uncertainty, plus measuring lagged relationships between two series
- **Regression, Filters & Frequency** - regression with autocorrelated errors, filtering and smoothing, and spectral methods for finding cycles

The emphasis is on doing the analysis in R and understanding what you're doing, not on mathematical derivation. Scattered *Asides* dig into the math and the R internals for readers who want them, but you can skip every one and still follow the core material.

## Who it's for

Graduate students and researchers in the environmental sciences who can get around in R and want to take time seriously as a dimension in their data. It assumes an introductory linear-modeling course and comfort with basic statistics. Everything time-series-specific is built up from there.

The example data lives in `data/`. Every build bundles it into a `data.zip` you can download straight from the site, so you can run the code against the exact data the chapters use.

## How it's built

The book is written in [Quarto](https://quarto.org) and rendered as a book site. Every push to `main` triggers a GitHub Actions workflow that renders the book and deploys it to [timeseries.andybunn.org](https://timeseries.andybunn.org/), so the live site is always current.

To build it locally you'll need R, Quarto, and the packages listed in `DESCRIPTION`. From the project root:

```sh
quarto render
```

Found an error or have a suggestion? Open an issue, or use the "Edit this page" link on any chapter. I'd love to hear from you.

## License

The book is dual-licensed:

- **Prose and figures** under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). Share and adapt for noncommercial use, keep derivatives open under the same terms, and give credit.
- **Code** under the [MIT License](https://opensource.org/licenses/MIT). Use it however you like.

*(`LICENSE.md` with the full text is coming as part of publication prep.)*
