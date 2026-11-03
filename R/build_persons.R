source("R/common.R")
t <- load_tilldelningar()
dir.create("person", showWarnings = FALSE)
ids <- unique(t$person_id)
for (id in ids) {
  pid <- id
  nm <- t[person_id == pid, namn][1]
  writeLines(c(
    "---",
    sprintf('title: "%s"', gsub('"', "'", nm)),
    "---",
    "",
    "```{r}",
    'source(if (file.exists("R/common.R")) "R/common.R" else "../R/common.R")',
    't <- load_tilldelningar()',
    sprintf('person_page(t, "%s", pfx = "../")', id),
    "```"
  ), file.path("person", paste0(id, ".qmd")), useBytes = TRUE)
}
stale <- setdiff(sub("\\.qmd$", "", list.files("person", pattern = "\\.qmd$")), ids)
if (length(stale)) file.remove(file.path("person", paste0(stale, ".qmd")))
cat("person pages:", length(ids), "\n")
