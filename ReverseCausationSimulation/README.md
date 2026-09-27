# Reverse causation simulator: NNS and BMI

A Shiny app for PUBHLTH430 (Non-Nutritive Sweeteners lecture) that students can
explore after class. In a simulated cohort, heavier people are more likely to
choose non-nutritive sweeteners (NNS). Students control the true effect of NNS
on BMI, the strength of that reverse causation, the error in measured baseline
BMI, and the cohort size. They then compare four estimates of the effect, each a
mean difference in 5-year BMI (NNS users minus non-users, kg/m², the same units
as McGlynn 2022 Figure 2):

| Design | Model | Data |
|---|---|---|
| Naive cohort | `bmi_followup ~ nns` | full cohort |
| Exclude people with obesity | `bmi_followup ~ nns` | measured baseline BMI < 30 |
| Adjust for baseline BMI | `bmi_followup ~ nns + bmi_measured` | full cohort |
| Simulated randomized trial | `bmi_followup ~ nns` | separate trial sample, 50:50 allocation |

The take-home points are:

- The naive cohort finds "harm" even when NNS has no effect.
- Excluding people with obesity shrinks the bias but doesn't remove it.
- Adjusting for baseline BMI works only if BMI is measured accurately.
- The trial is unbiased, though it still has sampling error.

**Implementation notes**

- Everything is in `app.R`. The simulation is a set of plain functions (`draw_random()`, `simulate_study()`, `estimate_effects()`), followed by the UI and server.
- The app uses **common random numbers**: each person's random draws are made once for a given seed and cohort size, and the sliders only transform those draws. So dragging a slider moves the estimates smoothly instead of reshuffling the sample.
- NNS use is `u < p_nns(bmi, k)` with `u ~ Uniform(0, 1)`, which is equivalent to `rbinom()`.
- The trial uses its own RNG stream (`seed + 1`), so the cohort sliders never change it.
- At the default seed (430) the unadjusted trial estimate is about +0.3 from the truth. That's ordinary sampling error (SE ≈ 0.15 at n = 5,000), and a "Try this" prompt asks students to click *New sample* and watch it wobble.

## Running and deployment

To run the app locally, install `shiny`, `bslib` and `ggplot2`, then run
`shiny::runApp("ReverseCausationSimulation")` from the repo root (or
`shiny::runApp()` from this folder). Run the tests, which encode the spec's
acceptance table (n = 5,000, seed 430), with
`testthat::test_dir("ReverseCausationSimulation/tests/testthat")`. The app
deploys automatically: every push to `main` runs the tests, exports the app with
[Shinylive](https://posit-dev.github.io/r-shinylive/) so R runs in the student's
browser via webR with no server, and publishes it to GitHub Pages at
`https://<github-username>.github.io/TeachingApps/ReverseCausationSimulation/`.
See `.github/workflows/deploy-shinylive.yml`. The first load takes about
15–30 seconds while R downloads into the browser.
