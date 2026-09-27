# TeachingApps
Various apps

## Apps

### [CRCScreening](CRCScreening/): colorectal cancer risk and screening

An R Shiny app for an in-class activity in public health / nutrition and
metabolism courses. Students use scenario cards, such as "28-year-old, father
diagnosed at 50, clinic offers Cologuard", to explore why the same screening
test means very different things at age 25 and at 55. They also see how family
history, Lynch syndrome (by gene), polygenic risk and lifestyle factors shift
absolute risk.

- **Testing tab:**
  - an icon array of 10,000 people
  - PPV, NPV and false positives per cancer found
  - pre- to post-test probability bars, including follow-up colonoscopy
  - PPV-by-age curves for each risk group
  - downstream colonoscopies and harms
  - optional Monte Carlo uncertainty
- **Sources tab:** every parameter with its citation and a list of the model's
  simplifying assumptions.
- **Planned:** tabs for prior elicitation, risk in context, and genetic testing
  as a test.

> **Not for diagnostic or clinical use.** The app is an illustrative
> teaching example. It uses a deliberately simplified model with draft,
> unverified numbers. It should not be used to estimate anyone's actual cancer
> risk, interpret a real test result, or make screening or treatment decisions.

All numbers live in `CRCScreening/data/parameters.csv`, and each is marked
`VERIFY` until it has been checked against the primary sources. Built with
bslib and Shiny modules; the core calculations are plain R functions with
testthat tests. See the [app README](CRCScreening/README.md) for details.

```r
shiny::runApp("CRCScreening")
```

### [ReverseCausationSimulation](ReverseCausationSimulation/): NNS and BMI

A simulated cohort for the Non-Nutritive Sweeteners lecture (PUBHLTH430).
Heavier people are more likely to choose NNS, so a naive cohort analysis makes
NNS look harmful even when it has no effect. Students move sliders for the true
effect, reverse-causation strength, BMI measurement error and cohort size. They
then compare four estimates: naive, excluding people with obesity at baseline,
adjusting for baseline BMI, and a simulated randomized trial. All data are simulated. See
the [app README](ReverseCausationSimulation/README.md).

```r
shiny::runApp("ReverseCausationSimulation")
```

## Live site

Every push to `main` runs the tests, builds each app with
[Shinylive](https://posit-dev.github.io/r-shinylive/) (R runs inside the
browser, so no server is needed), and publishes it to GitHub Pages at
`https://<github-username>.github.io/TeachingApps/`. Each app gets its own
subfolder, e.g. `.../TeachingApps/CRCScreening/`. The first load takes about
15–30 seconds while R downloads into the browser.

The workflow is in `.github/workflows/deploy-shinylive.yml`. Any top-level
folder with an `app.R` is published automatically. One-time setup:

1. Push this repo to GitHub as a public repository.
2. In the repo's **Settings → Pages**, set **Source** to **GitHub Actions**.
3. Push to `main`, or run the workflow from the **Actions** tab.
