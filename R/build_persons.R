suppressMessages(library(htmltools))
source("R/common.R")
t <- load_tilldelningar()
dir.create("person", showWarnings = FALSE)
ids <- unique(t$person_id)
for (id in ids) {
  pid <- id
  nm <- t[person_id == pid, namn][1]
  html <- as.character(person_page(t, id, pfx = "../"))
  writeLines(c(
    "---",
    sprintf('title: "%s"', gsub('"', "'", nm)),
    "---",
    "",
    "```{=html}",
    html,
    "```"
  ), file.path("person", paste0(id, ".qmd")), useBytes = TRUE)
}
stale <- setdiff(sub("\\.qmd$", "", list.files("person", pattern = "\\.qmd$")), ids)
if (length(stale)) file.remove(file.path("person", paste0(stale, ".qmd")))
cat("person pages:", length(ids), "\n")
