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
