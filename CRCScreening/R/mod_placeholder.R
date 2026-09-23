# Placeholder UI for tabs that are planned but not built yet -----------------

mod_placeholder_ui <- function(id, title, bullets) {
  card(
    card_header(title),
    p(class = "text-body-secondary", "Coming soon. Planned for this tab:"),
    tags$ul(lapply(bullets, tags$li))
  )
}
