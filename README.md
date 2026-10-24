# peditools2

Z-scores, percentiles and measurements for pediatric, neonatal and fetal growth charts and standards (CDC, WHO,
INTERGROWTH-21st and others) from their published parameters: the charts of [PediTools](https://peditools.org/), with
the same methods and the same results, for data analysis in R.

- **Vectorised** over measurements, ages, sexes, charts and measures: one call for a whole data frame.
- **The published methods by default**: the LMS method; WHO's restricted LMS method beyond ±3 SD where WHO
  prescribes it; CDC's extended BMI-for-age method above the 95th percentile; the INTERGROWTH-21st skew t (ST3) and
  normal models.
- **Every method can be switched**, and extrapolation is available, for studying how the charts behave.
- **The parameter tables themselves** (`growth_table()`, `growth_equations()`) and each chart's publications
  (`growth_references()`).
- **Curves and plots** (`growth_curves()`, and `growth_plot()` with ggplot2).
- **Your own charts** for the session (`growth_add_chart()`).

## Installation

From r-universe (binaries for Windows and macOS, and for WebAssembly):

```r
install.packages("peditools2", repos = c("https://jhchou.r-universe.dev", "https://cloud.r-project.org"))
```

or from GitHub, with the vignettes:

```r
remotes::install_github("jhchou/peditools2", build_vignettes = TRUE)
```

## Example

```r
library(peditools2)

# Weight 8.2 kg of a girl of 9.5 months on the CDC 2000 infant chart
growth_z(8.2, 9.5, "f", "cdc_2000_infant", "weight")
growth_p(8.2, 9.5, "f", "cdc_2000_infant", "weight")   # the percentile / 100

# The 3rd, 50th and 97th percentile weights at 12 months, WHO
growth_y(p = c(0.03, 0.5, 0.97), x = 12, sex = "m", chart = "who_2006_infant", measure = "weight")

# The charts, their units and coverage
growth_charts()
```

Measurements and ages are in each chart's own units (`growth_charts()`); `growth_convert()`, `growth_age_days()`,
`growth_corrected_age()` and `growth_pma()` convert them. See `vignette("peditools2")`, `vignette("methods")` and
`vignette("charts")`.

## Sources and checks

The parameter tables are those of PediTools, each checked row by row against its source; the package's calculations
are checked against the PediTools engine on every table. Each chart's sources and methods:
[PediTools Sources and methods](https://peditools.org/growth_engine/sources.php).

## Citation

Chou JH, Roumiantsev S, Singh R. PediTools Electronic Growth Chart Calculators: Applications in Clinical Care,
Research, and Quality Improvement. J Med Internet Res. 2020;22(1):e16204. doi:10.2196/16204
(`citation("peditools2")`).

## License

MIT. The successor of the `peditools` package (not compatible with it).
