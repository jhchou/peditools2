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
