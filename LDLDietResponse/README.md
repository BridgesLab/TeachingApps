# LDL-C diet response: why people differ

An R Shiny app for the NUTR630 lipid unit. You enter a theoretical diet
change, set the person's "knobs" (one per decoder site of the lipid unit),
and see the predicted change in LDL-C. You also see how much that outcome
varies, both between people and from uncertainty in the regression
coefficients.

> **Not for clinical use.** This is an illustrative teaching model built from
> linear regression coefficients. It should not be used to predict any real
> person's lipid response.

> **Every number is an unverified draft (`verified = n`).** The coefficient
> intervals are approximate. The knob sizes and their spread in the population
> are illustrative, not fitted to data.

## Running it

```r
# from this folder
shiny::runApp()

# tests
testthat::test_dir("tests/testthat")
```

Packages: shiny, bslib, ggplot2, withr, testthat. Files in `R/` are sourced
automatically. The public version is built with Shinylive; see the root README.

## What you set

- **Diet before and after.** "Before" defaults to a typical US diet. "After"
  can start from an example diet: Mediterranean-style, or one of four keto
  patterns (eggs and butter; coconut oil and butter; olive oil and fish;
  vegan). You then edit energy, protein, SFA (optionally split into
  lauric + myristic, palmitic and stearic), MUFA, PUFA, trans fat,
  cholesterol (with an egg counter, 186 mg each), soluble fiber and plant
  sterols. A food builder can fill these in from servings per day.
- **Baseline LDL-C.**
- **Person knobs:**

| Site | Knob | What it does in the model |
|---|---|---|
| A | fat on the plate | computed from the diet |
| B | cholesterol absorbed, 20–80% | scales the cholesterol and plant sterol terms |
| C | baseline synthesis (lathosterol ×typical) | how much of the extra absorbed cholesterol is offset by making less; less is offset when more arrives or there is less synthesis to switch off |
| D/E | "lean, very active, low-carb" switch: VLDL → LDL flux ×1–2.5 | multiplies production; labelled as the untested lipid energy model and off by default |
| F | APOE genotype; LDL receptor activity (FH-like demo) | scales every term |
| G | HDL and CETP | not modelled |

## What you see

**Predict tab**

- **Average person** (yellow): the regression prediction, with a 95% range
  from coefficient uncertainty.
- **This person**: the same diet with the knobs applied, with its own 95%
  range.
- **Across people**: the middle 90% of outcomes when B, C and F are drawn for
  2,000 simulated people, plus the SD and how many reach LDL-C 160+ or 190+.
- **Spread of outcomes**: a histogram of the simulated people, and a table of
  where the variance comes from. Rows cover coefficient uncertainty, all
  between-person differences, each knob alone (absorption, synthesis, APOE,
  LDL receptor), and everything together. On the eggs-and-butter keto diet,
  absorption and synthesis dominate (SD about 45 mg/dL vs 5 from the
  coefficients). On the coconut-oil diet, APOE and the LDL receptor do. For
  diets under 20% carbohydrate, the plot shades the individual rises seen in a
  real keto trial (Retterstøl 2018: +5% to +107%).
- **Waterfall**: each average-person term and each knob in mg/dL.
- **Benchmarks**: LDL-C categories and estimated ApoB.
- **Out-of-range warnings**: for example SFA above 20% of energy, or
  carbohydrate below 30%.

**Fix it tab:** the fix menu (swap the fat, fewer eggs, psyllium, plant
sterols, ezetimibe, statin, add carbs back), each tagged with its site, for
the average person and this person.

**Sources and assumptions tab:** every parameter with its 95% interval,
source and verified flag, plus the simplifications.

**Predict first** (sidebar) hides the answers until a guess is entered and
Reveal is clicked.

## The model

**Average person.** The full model (default) adds straight-line terms. SFA,
MUFA, PUFA and trans fat replacing carbohydrate (Mensink 2016: SFA +1.3, MUFA
−0.3, PUFA −0.8 mg/dL per 1% of energy; optional SFA subtypes), dietary
cholesterol (+2 mg/dL per 100 mg/day), soluble fiber (−2.2 per g) and plant
sterols (−4.5% per g, plateau at 3 g). "Simple" math is the class-card
formula, `(SFA% − 12) × 1.3 + (cholesterol − 300) ÷ 100 × 2`.

**This person.** LDL-C = production ÷ clearance. Each average term is turned
into a share of LDL receptor activity lost, so that with every knob at its
default the person equals the average person exactly. B and C scale the
cholesterol and sterol shares; F scales every share; D/E multiplies
production. Because receptor losses compound, a high absorber's rise
snowballs.

**Variance.**

- Coefficient uncertainty: each coefficient with a 95% interval in
  `parameters.csv` is drawn from a matching normal distribution
  (independently).
- Between people: absorption (logit-normal, 95% of people 25–75%), synthesis
  (log-normal), APOE genotype (population frequencies) and LDL receptor
  activity (log-normal) are drawn independently.
- Baseline LDL-C is held fixed and D/E is not drawn.
- The one-knob-at-a-time rows don't add up to the total, because the knobs
  interact.

## Files

```
LDLDietResponse/
├── app.R               page_navbar shell and server
├── data/
│   ├── parameters.csv  every number: value, 95% interval, unit, description, source, verified (y/n), notes
│   ├── diets.csv       typical US "before" diet and example "after" diets
│   └── foods.csv       food builder servings
├── R/
│   ├── params.R        load/validate data files, labels, decoder sites
│   ├── model.R         average terms, person model, compensation, ApoB, warnings
│   ├── spread.R        coefficient and between-person draws, variance breakdown
│   ├── diet.R          food builder and the fix menu
│   ├── plots.R         spread histogram, waterfall, fix bars (U-M colours)
│   └── ui.R            knob panels, diet editor, fix menu, sources tab
└── tests/testthat/     card formula, defaults = average, warnings, fixes, spread
```

## To verify before release

- The 95% intervals on every coefficient. The current ones are approximate.
- The dietary cholesterol coefficient: 2 mg/dL per 100 mg/day as on the class
  cards, while Weggemans 2001 gives 1.9 for LDL-C.
- Rouhani 2018 (+21.8 / −0.5 mg/dL): LDL-C or total cholesterol?
- The Mensink 2016 individual fatty acid coefficients; Brown 1999 (fiber);
  Gylling 2014 and Ras 2014 (plant sterols); APOE frequencies.
- The population spread of each knob, ideally against an egg-feeding or keto
  trial that reports individual responses.
