# CRC risk & screening: an in-class Shiny activity

Why does a positive stool test mean something very different at 25 than at 55,
and how do family history, Lynch syndrome, polygenic scores and lifestyle
change that? Students work through scenario cards on their own phones or
laptops and report back **false positives per cancer found** and a
**risk-equivalent age**.

> **Not for diagnostic or clinical use.** This app is an illustrative
> teaching example. It uses a deliberately simplified model with draft,
> unverified numbers. It should not be used to estimate anyone's actual cancer
> risk, interpret a real test result, or make screening or treatment decisions.

> **All numbers are draft placeholders marked `VERIFY`.** They are
> approximations from memory of the cited sources and must be checked against
> SEER, the Cologuard / Cologuard Plus trials (Imperiale 2014, 2024), FIT and
> Shield (Chung 2024) literature, the Prospective Lynch Syndrome Database, and
> USPSTF/ACS/USMSTF/NCCN guidance before classroom use.

## Running it

```r
# from this folder
shiny::runApp()

# tests
testthat::test_dir("tests/testthat")
```

Packages: shiny, bslib, dplyr, tidyr, purrr, ggplot2, readr, scales, withr,
testthat. Files in `R/` are sourced automatically by Shiny.

## Status

| Tab | Status |
|---|---|
| 0 · Before you start (prior elicitation) | placeholder |
| 1 · Risk in context | placeholder (calculation functions for it are written and tested in `R/risk.R`) |
| **2 · Testing** | **built** |
| 3 · Genetic testing as a test | placeholder (Lynch carrier prevalences are in the parameter file) |
| 4 · Sources and assumptions | built |

## File structure

```
CRCScreening/
├── app.R                  page_navbar shell; loads parameters and scenarios, wires modules
├── data/
│   ├── parameters.csv     every number in the app (one row per parameter)
│   └── scenarios.csv      classroom scenario cards (presets for the Testing tab)
├── R/
│   ├── params.R           load/validate the CSV, lookups, choice labels
│   ├── diagnostic.R       PPV/NPV, likelihood ratios, Bayes in odds form,
│   │                      sequential testing, three-state population model
│   ├── risk.R             age-specific hazard by risk group, cumulative risk,
│   │                      risk-equivalent age, pre-test prevalence, symptom LRs
│   ├── uncertainty.R      Beta helpers (mean/ESS, mean/CV), draws, summaries
│   ├── model.R            scenario pipeline: settings → inputs (point or draws) → outcomes;
│   │                      PPV-by-age curves
│   ├── format.R           "1 in X", percentages, counts
│   ├── plots.R            U-M brand palette, icon array, probability ladder,
│   │                      PPV curves, simulated PPV histogram
│   ├── mod_testing.R      Tab 2 module
│   ├── mod_sources.R      Tab 4 module (parameter table and assumptions)
│   └── mod_placeholder.R  stub for unbuilt tabs
└── tests/testthat/        diagnostic math, risk model, uncertainty, parameter file, pipeline
```

## Parameter file schema (`data/parameters.csv`)

One row per number. Long format so new tests, groups or ages are new rows, not
new code.

| column | meaning |
|---|---|
| `param_id` | unique key, e.g. `acc_fit_sens_crc`, `inc_avg_25_29`, `pen_MLH1_75` |
| `category` | `incidence`, `prevalence`, `test_accuracy`, `relative_risk`, `penetrance`, `genetics`, `harm`, `symptom`, `model`, `guideline` |
| `quantity` | e.g. `sens_crc`, `sens_aa`, `spec`, `rr_crc`, `rr_aa`, `cum_risk`, `likelihood_ratio` |
| `group` | risk group or factor (`average`, `fdr`, `prs_top10`, `lynch_MLH1`, `obesity`, …) |
| `test` | `cologuard`, `cologuard_plus`, `fit`, `shield`, `colonoscopy` |
| `target` | `any`, `crc` (cancer only), `crc_aa` (cancer + advanced adenoma) |
| `age_min`, `age_max` | age band (for FDR rows: the *relative's* age at diagnosis) |
| `value` | point estimate |
| `dist` | `beta`, `lognormal` or `fixed`: the uncertainty distribution |
| `alpha`, `beta` | Beta shape parameters (Jeffreys: successes + 0.5, failures + 0.5) |
| `lower`, `upper` | 95% interval for lognormal parameters (relative risks, harms, LRs) |
| `units`, `source`, `status`, `notes` | provenance; `status` is `VERIFY` until checked |

`validate_params()` refuses to start the app if ids are duplicated, values are
missing, or Beta/lognormal parameters are malformed. A test asserts that every
row is still `VERIFY`; change it once you have checked the values.

## How the model works

1. **Pre-test probability.** Average-risk annual incidence comes from SEER
   5-year bands, interpolated log-linearly. FDR, polygenic and lifestyle
   profiles multiply it by relative risks. Lynch syndrome uses a Weibull curve
   through the cumulative risk at 50 and 75 for each gene. Prevalence of
   undiagnosed, screen-detectable cancer = incidence × mean sojourn time.
   Symptoms update the odds with a likelihood ratio. Advanced-adenoma
   prevalence is looked up by age band × a group multiplier.
2. **Three states, not two.** People have cancer, an advanced adenoma, or
   neither. Stool and blood tests react to adenomas, so when the target is
   "cancer only", an adenoma-positive counts as a false positive for cancer
   (the app notes that finding it is still useful).
3. **Outcomes per 10,000:** TP/FP/FN/TN, PPV, NPV, false positives per true
   positive, colonoscopies, perforations, bleeds, cancers found and missed,
   and the PPV after colonoscopy + biopsy (assuming conditional independence).
4. **Uncertainty.** Sensitivity and specificity are drawn from their Beta
   distributions (shared across ages within a simulation), prevalence from a
   Beta with the CV in the file, and harms from lognormals. The same
   `evaluate_scenario()` runs on point values or draws.

**Bayesian extension point:** `scenario_inputs()` returns a tibble of draws
with columns `draw, age, prev_crc, prev_aa, p_crc, p_aa, p_none, s_*, c_*,
perf, bleed`. A brms model (e.g. a hierarchical model of FIT sensitivity across
studies, or of incidence by age) only needs to produce posterior draws in that
shape; everything downstream stays the same.

## Classroom use

Scenario cards (in `data/scenarios.csv`, selectable in the Testing tab):

- **A.** 25, no symptoms, orders Cologuard from an ad
- **B.** 28, father diagnosed at 50, clinic offers Cologuard
- **C.** 24, MLH1 Lynch variant, asks whether a yearly FIT is enough
- **D.** 27, rectal bleeding + iron-deficiency anemia, negative FIT (diagnostic, not screening)
- **E.** 35, top-decile polygenic score + heavy alcohol, asks for Shield
- **F.** 55, average risk, FIT (contrast with A)

Each card has a "report back" prompt. Groups report false positives per cancer
found and the risk-equivalent age.

## Known simplifications

These are listed in full in the Sources tab. The main ones:

- one-time testing only
- test accuracy comes from trials in people 40–50+
- relative risks are constant across age
- lifestyle factors multiply together
- symptom odds ratios are used as likelihood ratios
- competing mortality is ignored
- everyone with a positive test is assumed to get a colonoscopy

## Roadmap

1. Tab 0: prior elicitation (guess → reveal). Guesses stay in the session,
   with an optional class histogram, which needs shared storage such as a
   Google Sheet or a small database.
2. Tab 1: cumulative risk curves, risk-equivalent age vs guideline start
   ages, relative vs absolute risk at 25 vs 55, and an optional birth-cohort
   panel. The functions already exist in `R/risk.R`.
3. Tab 3: Lynch panel as a screening test. Per-gene carrier prevalence is
   already in the parameter file; still to add are VUS rate and
   analytic/clinical validity parameters, plus cascade testing (50% prior in
   first-degree relatives).
4. An optional brms module that replaces the Beta priors with posterior draws.
