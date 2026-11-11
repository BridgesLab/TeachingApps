# UI pieces --------------------------------------------------------------------

site_badge <- function(letter) {
  tags$span(class = "site-badge", letter)
}

site_title <- function(letter) {
  s <- SITES[match(substr(letter, 1, 1), SITES$letter), ]
  tagList(site_badge(letter), tags$span(class = "ms-1", s$name),
          tags$small(class = "text-muted ms-1", paste0("(", s$lecture, ")")))
}

APP_CSS <- "
.site-badge{display:inline-flex;align-items:center;justify-content:center;min-width:1.5em;height:1.5em;
  padding:0 .3em;border-radius:.75em;background:#00274C;color:#FFCB05;font-weight:700;font-size:.85em}
.avg-panel{background:#FFCB05;color:#00274C}
.diet-grid .form-group{margin-bottom:.35rem}
.diet-grid label{font-size:.85rem}
.lab-table td,.lab-table th{padding:.15rem .5rem}
.small-help{font-size:.82rem;color:#555}
"

#' Numeric input for one diet field
diet_input <- function(prefix, field, label, value, step = 1, min = 0, max = NA) {
  numericInput(paste0(prefix, field), label, value = value, min = min, max = max, step = step)
}

#' A column of diet inputs (prefix "b_" or "a_")
diet_column <- function(prefix, d, title) {
  tagList(
    tags$h6(title, class = "fw-bold"),
    diet_input(prefix, "energy", "Energy (kcal/day)", d$energy, step = 50),
    diet_input(prefix, "protein", "Protein (% energy)", d$protein),
    diet_input(prefix, "sfa", "SFA (% energy)", d$sfa, step = 0.5),
    conditionalPanel(
      "input.split", class = "ps-2 border-start",
      diet_input(prefix, "sfa_lm", "of which lauric + myristic", d$sfa_lm, step = 0.5),
      diet_input(prefix, "sfa_p", "of which palmitic", d$sfa_p, step = 0.5),
      diet_input(prefix, "sfa_s", "of which stearic", d$sfa_s, step = 0.5)
    ),
    diet_input(prefix, "mufa", "MUFA (% energy)", d$mufa, step = 0.5),
    diet_input(prefix, "pufa", "PUFA (% energy)", d$pufa, step = 0.5),
    diet_input(prefix, "trans", "Trans fat (% energy)", d$trans, step = 0.5),
    diet_input(prefix, "chol", "Cholesterol (mg/day)", d$chol, step = 10),
    div(class = "mb-2",
        actionButton(paste0(prefix, "egg_minus"), "− egg", class = "btn-sm btn-outline-primary"),
        actionButton(paste0(prefix, "egg_plus"), "+ egg", class = "btn-sm btn-outline-primary"),
        tags$span(class = "small-help ms-1", "186 mg each")),
    diet_input(prefix, "fiber", "Soluble fiber (g/day)", d$fiber, step = 0.5),
    diet_input(prefix, "sterol", "Plant sterols/stanols (g/day)", d$sterol, step = 0.1),
    uiOutput(paste0(prefix, "macros"))
  )
}

diet_card <- function(before, after, foods, diets, selected) {
  examples <- diets[diets$diet_id != "typical_us", ]
  card(
    card_header("Diet: before vs after"),
    selectInput("example", "Start 'after' from an example diet (then edit)", selected = selected,
                choices = c("Choose…" = "", stats::setNames(examples$diet_id, examples$label))),
    uiOutput("example_text"),
    checkboxInput("before_fixed", "'Before' is the typical US diet (12% SFA, 300 mg cholesterol)", TRUE),
    checkboxInput("split", "Split SFA by fatty acid (full model only)", FALSE),
    layout_columns(
      col_widths = c(6, 6), class = "diet-grid",
      div(conditionalPanel("!input.before_fixed", diet_column("b_", before, "Before")),
          conditionalPanel("input.before_fixed", uiOutput("before_summary"))),
      diet_column("a_", after, "After")
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "Build the 'after' plate from foods",
        p(class = "small-help",
          "Servings per day added to the 'before' diet. Each serving's fat replaces the same energy from carbohydrate foods, and fiber from those foods falls in proportion."),
        layout_columns(
          col_widths = c(6, 6),
          !!!lapply(seq_len(nrow(foods)), function(i) {
            numericInput(paste0("food_", foods$food_id[i]),
                         paste0(foods$label[i], " (", foods$serving[i], ")"),
                         value = 0, min = 0, max = 20, step = 1)
          })
        ),
        actionButton("apply_foods", "Use these foods for 'after'", class = "btn-primary btn-sm")
      )
    )
  )
}

knob_panels <- function(p) {
  accordion(
    id = "knobs_acc", open = c("B", "C", "F"), multiple = TRUE,
    accordion_panel(
      title = site_title("A"), value = "A",
      p(class = "small-help", "Computed from the diet, not a knob."),
      uiOutput("site_a_text")
    ),
    accordion_panel(
      title = site_title("B"), value = "B",
      sliderInput("abs", "Cholesterol absorbed (%)", min = 100 * p[["abs_min"]],
                  max = 100 * p[["abs_max"]], value = 100 * p[["abs_avg"]], step = 1, post = "%"),
      p(class = "small-help",
        "NPC1L1 brings cholesterol in; ABCG5/8 pumps it back out. People range from about 20% to 80%.")
    ),
    accordion_panel(
      title = site_title("C"), value = "C",
      sliderInput("synth", "Baseline synthesis: lathosterol (×typical)", min = p[["synth_min"]],
                  max = p[["synth_max"]], value = 1, step = 0.1, post = "×"),
      uiOutput("comp_text"),
      p(class = "small-help",
        "How far HMG-CoA reductase can fall when cholesterol arrives. More synthesis to switch off, or less cholesterol arriving (site B), means more of the extra is offset.")
    ),
    accordion_panel(
      title = tagList(site_badge("D/E"), tags$span(class = "ms-1", "VLDL output and LPL unloading")),
      value = "DE",
      checkboxInput("de_on", "Lean, very active, low-carb", FALSE),
      div(class = "alert alert-warning small py-1 px-2 mb-2",
          strong("Untested hypothesis: "), "the lipid energy model (Norwitz 2022). Off for the average person."),
      conditionalPanel(
        "input.de_on",
        sliderInput("de_flux", "VLDL → LDL flux (×usual)", min = 1, max = p[["vldl_flux_max"]],
                    value = 2, step = 0.05)
      )
    ),
    accordion_panel(
      title = site_title("F"), value = "F",
      selectInput("apoe", "APOE genotype", choices = APOE_CHOICES, selected = "e3e3"),
      sliderInput("ldlr", "LDL receptor activity (% of typical)", min = 100 * p[["ldlr_min"]],
                  max = 150, value = 100, step = 5, post = "%"),
      actionLink("fh_demo", "FH-like setting (instructor demo)")
    ),
    accordion_panel(
      title = site_title("G"), value = "G",
      p(class = "small-help",
        "HDL and CETP are not in this model. HDL-C marks risk, but raising it does not lower LDL-C or risk.")
    )
  )
}

fix_controls <- function() {
  card(
    card_header("Fix menu: changes on top of the 'after' diet"),
    sliderInput("fx_swap", tagList(site_badge("A"), " Swap SFA out (% of energy)"),
                min = 0, max = 20, value = 0, step = 1),
    radioButtons("fx_swap_to", NULL, inline = TRUE, selected = "nut",
                 choices = c("Nut butters (MUFA + PUFA)" = "nut", "Olive oil, avocado (MUFA)" = "mufa",
                             "Walnuts, seeds (PUFA)" = "pufa")),
    sliderInput("fx_eggs", tagList(site_badge("B"), " Fewer eggs per day"), min = 0, max = 6, value = 0, step = 1),
    sliderInput("fx_psyllium", tagList(site_badge("C"), " Psyllium: soluble fiber added (g/day)"),
                min = 0, max = 10, value = 0, step = 0.5),
    checkboxInput("fx_sterols", tagList(site_badge("B"), " Plant sterol spread (2 g/day)")),
    checkboxInput("fx_ezetimibe", tagList(site_badge("B"), " Ezetimibe")),
    checkboxInput("fx_statin", tagList(site_badge("C"), " Statin (moderate intensity)")),
    checkboxInput("fx_carbs", tagList(site_badge("D/E"), " Add carbs back (100 g/day)")),
    p(class = "small-help",
      "The diet change uses the math chosen in the sidebar. The fixes themselves are scored with the full Mensink model, so a fat swap counts both the SFA removed and the MUFA/PUFA added (1.6–2.1 mg/dL per 1% of energy), as on the fix menu.")
  )
}

ASSUMPTIONS <- c(
  "Average person: straight-line regression terms added together. The full model counts SFA, MUFA, PUFA and trans fat in place of carbohydrate, the SFA subtypes (if split), dietary cholesterol, soluble fiber and plant sterols. Simple math is the class-card formula (SFA and cholesterol only).",
  "The regression coefficients come from trials of ordinary diets. Keto diets (SFA above 20%, carbohydrate near 10%) are outside those ranges, so every keto prediction is an extrapolation.",
  "This person: LDL-C = production ÷ clearance. Each average term is converted into a share of LDL receptor activity lost (or gained), so with every knob at its default the person equals the average person exactly.",
  "B (absorption) and C (compensation) scale the cholesterol and plant sterol shares by (absorption ÷ 50%) × (1 − compensation) ÷ (1 − 70%). Compensation = 1 − 30% × (absorption ratio ÷ baseline lathosterol)^1.75: it falls when more arrives or there is less synthesis to switch off. F (APOE, LDL receptor activity) scales every share. D/E multiplies production.",
  "The knob sizes (compensation, APOE multipliers, VLDL flux) and their spread in the population are illustrative, not fitted to data.",
  "Waterfall knob bars switch the knobs on in decoder order (B, C, D/E, F). Because receptor losses compound, a knob's bar depends on the knobs set before it.",
  "Coefficient uncertainty: each regression coefficient with a 95% interval in the parameter file is drawn from a normal distribution matching that interval, independently of the others (in reality they are correlated). The intervals are approximate.",
  "Differences between people: absorption, synthesis, APOE genotype and LDL receptor activity are drawn independently (in reality high absorbers tend to be low synthesizers). Baseline LDL-C is held fixed, and D/E is not drawn.",
  "The one-knob-at-a-time rows vary a single knob with the others at the average. Because the knobs interact, their variances do not add up to the total.",
  "ApoB is estimated from LDL-C with one straight line. Real ApoB depends on how much cholesterol each particle carries.",
  "Ezetimibe's effect scales with absorption; statin effect is a fixed percentage. Drug effects multiply the diet prediction.",
  "No time course, no weight change, no HDL, TG or CETP effects, no measurement error."
)

sources_panel <- function(params) {
  layout_columns(
    col_widths = 12,
    card(
      card_header("Every number in this app, and where it comes from"),
      div(class = "alert alert-warning mb-2",
          strong("Unverified draft values. "),
          "verified = n until checked against the primary source. Edit data/parameters.csv to change any number; the app reads it on start-up."),
      selectInput("param_cat", "Show category", width = "280px",
                  choices = c("All" = "", sort(unique(params$category)))),
      div(style = "overflow-x:auto;", tableOutput("param_table"))
    ),
    card(card_header("Simplifying assumptions"), tags$ol(lapply(ASSUMPTIONS, tags$li)))
  )
}
