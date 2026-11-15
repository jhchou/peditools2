# peditools2 0.3.0

Four growth assessment tools, each as its PediTools calculator: mid-parental height, height velocity, predicted
adult height and WHO growth increments.

- `growth_height_target()`: mid-parental (target) height from both parents' heights, by Tanner 1970, Hermanussen
  and Cole 2003, Wright and Cheetham 1999, the Dutch JGZ guideline 2019 and Luo 1998, with each method's range, the
  SDS on an adult reference (CDC 2000 at 20 years or WHO 2007 at 19 years) and, optionally, the child's height SDS
  from 2 years and its difference from the target; as the PediTools mid-parental height calculator.
- Kelly 2014's annual height velocity reference for US children: `kelly_2014` (all children) and
  `kelly_2014_earlier`, `kelly_2014_average`, `kelly_2014_later` (by puberty onset), measure `height_velocity` in
  cm/year by the interval's mid-age in years, as published (the average-maturing boys' row at 17.75 years, M printed
  0.0, left out). The paper's columns labelled 10th and 90th are the 9th and 91st centiles; the L, M, S are consistent.
- `growth_height_velocity()`: the velocity between two measurements from 2 years (at least 6 months apart) and its SDS
  on Kelly's tables at the mid-age, flagged when the interval is outside Kelly's 11 to 13 months; as the PediTools
  height velocity calculator.
- `growth_charts()`: `calculators` now also names the height assessment tools that use a chart (mid-parental height,
  height velocity).
- `growth_target_height()`, in the public development version 0.2.0.9000, is renamed `growth_height_target()`, to go
  with `growth_height_velocity()` and `growth_height_adult()` (no alias: it was never in a release).
- `growth_height_adult()`: predicted adult height by Bayley-Pinneau 1952 (from a Greulich-Pyle bone age; the table by
  the bone age's lead or lag, the average table, flagged, where the matching one does not cover it) and Khamis-Roche
  1994 with the coefficients of its 1995 erratum (from weight and both parents' heights, 4 to 17.5 years; its 50% and
  90% error bounds by age read from the paper's figures); as the PediTools predicted adult height calculator. The
  tables come from the PediTools repository unchanged (transcribed twice, independently, from the papers).
- `growth_bone_age()`: reads bone ages typed in years and months ("11-6", "11y 6m") into months, refusing decimals.
- `growth_increments()`: WHO growth increments, 0 to 24 months (WHO 2009): the gain in weight, length and head
  circumference between two visits with its Z-score by WHO's restricted LMS method (weight shifted by delta), on WHO's
  intervals when the visits are within WHO's tolerance, else WHO's suggestion (the tabulated interval, or the gain
  prorated to it), and at the exact ages with L, M, S and delta interpolated (along the start month, WHO's "more
  precise option"; between interval lengths, PediTools' extension); by corrected age too below 37 weeks; as the
  PediTools WHO growth increments calculator. The parameters come from the PediTools repository unchanged (WHO's 26
  spreadsheets, checked row by row).
- `growth_height_velocity()`: the interval limits as lenient as calendar months allow, as on the website since
  2026-11-15: 6 months from 180 days (was 182.625), the 11 and 13 months of Kelly's annual interval at 333 and 398
  days (were 334.8 and 395.7), the under-12-months note below 364 days (was 365.25). The velocity and SDS are
  unchanged.

# peditools2 0.2.0

- Two Noonan syndrome charts: `malaquias_2012` (Brazil; height, weight and BMI, 0-20 years) and `isojima_2016`
  (Japan; the same measures and ages). Malaquias 2012 published centiles only: its L, M, S were fitted to them at
  each age, reproducing every published centile within its rounding; in the girls' tables the column labelled 90th
  is the +1 SD curve and was fitted as such (described on the PediTools Sources and methods page,
  https://peditools.org/growth_engine/sources.php).

# peditools2 0.1.0

The first public version, the successor of `peditools` (written anew; not compatible with it).

- Z-scores, probabilities and measurements: `growth_z()`, `growth_p()`, `growth_y()`, `growth_detail()`, vectorised
  over every argument.
- Methods: LMS; WHO's restricted LMS method beyond ±3 SD (`sd23`); CDC's extended BMI-for-age method (`cdc_bmi_ext`);
  the INTERGROWTH-21st ST3 and normal models. The published method by default; each can be switched.
- Coverage and extrapolation (`extrapolate`: `"coverage"`, `"none"`, `"linear"`).
- Parameters: `growth_charts()`, `growth_table()`, `growth_equations()`, `growth_params()`; `growth_lms_z()`,
  `growth_lms_y()`, `growth_st3_z()`, `growth_st3_y()` on parameters given directly.
- References: `growth_references()`, `growth_data_version()`.
- Curves and plots: `growth_curves()`, `growth_plot()` (ggplot2).
- Your own charts: `growth_add_chart()`, `growth_remove_chart()`.
- Helpers: `growth_convert()`, `growth_age_days()`, `growth_corrected_age()`, `growth_pma()`.
